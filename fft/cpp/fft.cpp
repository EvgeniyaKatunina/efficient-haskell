#include <iostream>
#include <fstream>
#include "bitmap.h"
#include "wavefile.h"
#include <unordered_map>
#include <cmath>
#include <vector>
#include <complex>
#include <algorithm>

#define MATH_PI (3.14159265358979323846)

using namespace std;

typedef complex<double> cdouble;

const uint32_t fastalloc_size = 1024 * 1024 * 16;
uint8_t fastalloc_data[fastalloc_size];
uint32_t fastalloc_pointer = 0;

unordered_map<int, unordered_map<int, double>> icost, isint;
unordered_map<int, unordered_map<int, cdouble>> iexpt;

void make_itrig(int len) {
    if(icost.find(len) != icost.end())
        return;
    for(int i = 0; i < len; i++) {
        for(int j = 0; j < len; j++) {
            auto cv = cos(-2 * MATH_PI * i * j / len);
            auto sv = sin(-2 * MATH_PI * i * j / len);
            icost[len][i * j] = cv;
            isint[len][i * j] = sv;
            iexpt[len][i * j] = cdouble(cv, sv);
        }
    }
}

double ifcos(int i, int j, int len) {
    return icost[len][i * j];
}

double ifsin(int i, int j, int len) {
    return isint[len][i * j];
}

cdouble ifexp(int i, int j, int len) {
    return iexpt[len][i * j];
}

unordered_map<int, vector<double>> fwndt;

inline double ach(double x) {
    return log(x + sqrt(x * x - 1));
}

inline double ch(double x) {
    return (exp(x) + exp(-x)) / 2;
}

inline double cheby_poly(int n, double x) {
    if(x >= -1 && x <= 1)
        return cos(n * acos(x));
    return ch(n * ach(x));
}

inline void cheby_win(vector<double>& out, int N, double atten) {
    int nn = 0;
    double n = 0;
    double M = 0;
    double sum = 0, maxy = 0;
    double tg = pow(10.0, atten / 20.0);
    double x0 = ch(ach(tg)/(N-1));
    M = (N-1)/2.0 + 0.5;
    while(nn < (N/2 + 1)) {
        n = nn - M;
        sum = 0;
        for(int i = 1; i <= M; i++) {
            sum += cheby_poly(N - 1, x0 * cos(MATH_PI * i / N)) * cos(2 * MATH_PI * n * i / N);
        }
        out[nn] = tg + 2 * sum;
        out[N - nn - 1] = out[nn];
        if(out[nn] > maxy)
            maxy = out[nn];
        nn++;
    }
    out[N] = 0;
    for(int i = 0; i <= N; i++) {
        out[i] /= maxy;
    }
}

void make_fwnd(int len) {
    if(fwndt.find(len) != fwndt.end())
        return;
    
    fwndt[len].resize(len + 1);
    cheby_win(fwndt[len], len, 50.0);
}

inline double fwnd(int i, int len) {
    return fwndt[len][i];
}

void dft2(vector<double>& arr, int pos, int len, int step, vector<cdouble>& out, int off) {
    make_itrig(len);
    make_itrig(len * step);
    make_fwnd(len * step);
    
    for(int i = 0; i < len; i++) {
        cdouble c;
        for(int j = 0; j < len; j++) {
            double coef = arr[pos + j * step] * fwnd(j * step + off, len * step);
            c += ifexp(i, j, len) * coef;
        }
        out[i] = c;
    }
}

void ifft(vector<double>& arr, int pos, int len, int step, vector<cdouble>& inout, int off) {
    if(len <= 2) {
        dft2(arr, pos, len, step, inout, off);
        return;
    }
    
    make_itrig(len);
    
    ifft(arr, pos + step, len / 2, step * 2, inout, off + step);
    for(int i = 0; i < len / 2; i++) {
        inout[i + len / 2] = inout[i]; // todo: this can be actually optimized using pointer magic
    }
    
    ifft(arr, pos, len / 2, step * 2, inout, off);
    
    for(int i = 0; i < len / 2; i++) {
        auto t = inout[i];
        auto ic = ifexp(i, 1, len);
        
        auto x = inout[i + len / 2];
        
        inout[i] = t + ic * x;
        inout[i + len / 2] = t - ic * x;
    }
}

void fft(vector<double>& arr, int pos, int len, bool phases, vector<cdouble>& out, vector<double>& magout, vector<double>& phaseout) {
    make_itrig(len);
    make_fwnd(len);
    ifft(arr, pos, len, 1, out, 0);
    for(int i = 0; i < len/2; i++) {
        magout[i] = abs(out[i]);
        if(phases)
            phaseout[i] = arg(out[i]);
    }
}

void subspectrum(vector<double>& arr, int window, int start, int end, int wide, int tall, string fn, int dc = 1) {
    auto cramming_factor = (end - start) / (double) wide / dc;
    bitmap bmp(wide, tall, fn);
    vector<cdouble> storage;
    storage.resize(window);
    vector<double> mags;
    vector<double> dummy;
    mags.resize(window);
    for(int i = start; i < end; i += dc) {
        fft(arr, i, window, false, storage, mags, dummy);
        for(int j = 0; j < window / 2; j++) {
            int cpx = (i - start) * wide / (end - start);
            int cpy = j * tall * 2 / window;
            
            double cf = 0.15 / cramming_factor;
            
            #define mf1(x) log(x + 1)
            
            bmp(cpx, cpy).r += mf1(mags[j] / cf) * cf;
            bmp(cpx, cpy).g += mf1(mags[j] / cf) * cf;
            bmp(cpx, cpy).b += mf1(mags[j] / cf) * cf;
            
            #undef mf1
        }
    }
    for(int cc = 0; cc < wide; cc++) {
        double average = 1;
        double maxx = 1.0;
        for(int cr = 0; cr < tall; cr++) {
            average = min(average, bmp(cc, cr).r);
        }
        for(int cr = 0; cr < tall; cr++) {
            auto nv = bmp(cc, cr).r - average;
            nv = max(0.0, nv);
            nv /= maxx - average;
            bmp(cc, cr).r = nv;
            bmp(cc, cr).g = nv;
            bmp(cc, cr).b = nv;
        }
    }
    bmp.writeAll();
}

int main() {
    auto wav = readWAV("../Toumei Elegy.wav");

    vector<double> ds;
    ds.reserve(wav.channels[0].size());
    for(int i = 0; i < wav.channels[0].size(); i++) {
        ds.push_back(wav.channels[0][i] / 65536.0);
    }
    
    subspectrum(ds, 1024, wav.frequency * 20, wav.frequency * 30, 2200, 512, "../spectrum_cpp.bmp", 25);
    
    return 0;
}
