using System;
using System.Collections.Generic;
using System.Numerics;

public class Fft {
    private static Dictionary<int, Dictionary<int, Complex>> iexpt = new Dictionary<int, Dictionary<int, Complex>>();

    private static void make_itrig(int len) {
        if(iexpt.ContainsKey(len))
            return;
        iexpt[len] = new Dictionary<int, Complex>();
        for(int i = 0; i < len; i++) {
            for(int j = 0; j < len; j++) {
                var cv = Math.Cos(-2 * Math.PI * i * j / len);
                var sv = Math.Sin(-2 * Math.PI * i * j / len);
                iexpt[len][i * j] = new Complex(cv, sv);
            }
        }
    }

    private static Complex ifexp(int i, int j, int len) {
        return iexpt[len][i * j];
    }

    private static Dictionary<int, double[]> fwndt = new Dictionary<int, double[]>();

    private static double ach(double x) {
        return Math.Log(x + Math.Sqrt(x * x - 1));
    }

    private static double ch(double x) {
        return (Math.Exp(x) + Math.Exp(-x)) / 2;
    }

    private static double cheby_poly(int n, double x) {
        if(x >= -1 && x <= 1)
            return Math.Cos(n * Math.Acos(x));
        return ch(n * ach(x));
    }

    private static void cheby_win(double[] arr, int N, double atten) {
        int nn = 0;
        double n = 0;
        double M = 0;
        double sum = 0, maxy = 0;
        double tg = Math.Pow(10.0, atten / 20.0);
        double x0 = ch(ach(tg)/(N-1));
        M = (N-1)/2.0 + 0.5;
        while(nn < (N/2 + 1)) {
            n = nn - M;
            sum = 0;
            for(int i = 1; i <= M; i++) {
                sum += cheby_poly(N - 1, x0 * Math.Cos(Math.PI * i / N)) * Math.Cos(2 * Math.PI * n * i / N);
            }
            arr[nn] = tg + 2 * sum;
            arr[N - nn - 1] = arr[nn];
            if(arr[nn] > maxy)
                maxy = arr[nn];
            nn++;
        }
        arr[N] = 0;
        for(int i = 0; i <= N; i++) {
            arr[i] /= maxy;
        }
    }

    private static void make_fwnd(int len) {
        if(fwndt.ContainsKey(len))
            return;

        fwndt[len] = new double[len + 1];
        cheby_win(fwndt[len], len, 50.0);
    }

    private static double fwnd(int i, int len) {
        return fwndt[len][i];
    }

    private static void dft2(double[] arr, int pos, int len, int step, Complex[] @out, int off) {
        make_itrig(len);
        make_itrig(len * step);
        make_fwnd(len * step);
    
        for(int i = 0; i < len; i++) {
            Complex c = new Complex(0, 0);
            for(int j = 0; j < len; j++) {
                double coef = arr[pos + j * step] * fwnd(j * step + off, len * step);
                c += ifexp(i, j, len) * coef;
            }
            @out[i] = c;
        }
    }

    private static void ifft(double[] arr, int pos, int len, int step, Complex[] inout, int off) {
        if(len <= 2) {
            dft2(arr, pos, len, step, inout, off);
            return;
        }
    
        make_itrig(len);
    
        ifft(arr, pos + step, len / 2, step * 2, inout, off + step);
        for(int i = 0; i < len / 2; i++) {
            inout[i + len / 2] = inout[i];
        }
    
        ifft(arr, pos, len / 2, step * 2, inout, off);
    
        for(int i = 0; i < len / 2; i++) {
            var t = inout[i];
            var ic = ifexp(i, 1, len);
        
            var x = inout[i + len / 2];
        
            inout[i] = t + ic * x;
            inout[i + len / 2] = t - ic * x;
        }
    }

    private static void fft(double[] arr, int pos, int len, bool phases, Complex[] @out, double[] magout, double[] phaseout) {
        make_itrig(len);
        make_fwnd(len);
        ifft(arr, pos, len, 1, @out, 0);
        //dft2(arr, pos, len, 1, out, 0);
        for(int i = 0; i < len/2; i++) {
            magout[i] = @out[i].Magnitude;
            if(phases)
                phaseout[i] = @out[i].Phase;
        }
    }

    private static void subspectrum(double[] arr, int window, int start, int end, int wide, int tall, string fn, int dc = 1) {
        var cramming_factor = (end - start) / wide / dc;
        var bmp = new Bitmap(wide, tall, fn);
        var storage = new Complex[window];
        var mags = new double[window];
        var dummy = new double[1];
        for(int i = start; i < end; i += dc) {
            fft(arr, i, window, false, storage, mags, dummy);
            for(int j = 0; j < window / 2; j++) {
                int cpx = (i - start) * wide / (end - start);
                int cpy = j * tall * 2 / window;
            
                double cf = 0.15 / cramming_factor;
            
                bmp.pixels[bmp.idx(cpx, cpy)].r += Math.Log(1 + mags[j] / cf) * cf;
            }
        }
        for(int cc = 0; cc < wide; cc++) {
            double average = 1;
            double maxx = 1.0;
            for(int cr = 0; cr < tall; cr++) {
                average = Math.Min(average, bmp.pixels[bmp.idx(cc, cr)].r);
            }
            for(int cr = 0; cr < tall; cr++) {
                var nv = bmp.pixels[bmp.idx(cc, cr)].r - average;
                nv = Math.Max(0.0, nv);
                nv /= maxx - average;
                bmp.pixels[bmp.idx(cc, cr)].r = nv;
                bmp.pixels[bmp.idx(cc, cr)].g = nv;
                bmp.pixels[bmp.idx(cc, cr)].b = nv;
            }
        }
        bmp.writeAll();
    }

    public static void Main(string[] args) {
        var wav = WaveReader.readWAV("../Toumei Elegy.wav");
        
        var ds = new double[wav.channels[0].Length];
        for(int i = 0; i < wav.channels[0].Length; i++) {
            ds[i] = wav.channels[0][i] / 65536.0;
        }
    
        subspectrum(ds, 1024, wav.frequency * 20, wav.frequency * 30, 2200, 512, "../spectrum_cs.bmp", 25);
    }
}