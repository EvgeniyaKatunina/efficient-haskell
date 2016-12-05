#include <algorithm>
#include <cstring>
#include <sstream>
#include <iostream>
#include <chrono>
#include <cmath>
#include <unordered_set>
#include <set>
#include <iomanip>

using namespace std;

#define N 200000

int list1[N];
int list2[N];
int list3[N];
int list4[N];
int list5[N];
int list6[N];
int list [N];

void copyList(int* otherList) {
    memcpy(list, otherList, sizeof(int) * N);
}

int nubSort() {
    sort(begin(list), end(list));
    return (int) (unique(begin(list), end(list)) - begin(list));
}

int nubHash() {
    unordered_set<int> set;
    int pos = 0;
    for (int i = 0; i < N; i++) {
        if (set.count(list[i]) == 0) {
            set.insert(list[i]);
            list[pos++] = list[i];
        }
    }
    return pos;
}

int nubTree() {
    set<int> set;
    int pos = 0;
    for (int i = 0; i < N; i++) {
        if (set.count(list[i]) == 0) {
            set.insert(list[i]);
            list[pos++] = list[i];
        }
    }
    return pos;
}

void printList(int* lst) {
    for (int i = 0; i < N; i++) {
        printf("%d, ", lst[i]);
    }
    printf("\n");
}

template <class Tp>
inline __attribute__((always_inline)) void DoNotOptimize(Tp const& value) {
    asm volatile("" : : "g"(value) : "memory");
}

int index = 0;
string next() {
    stringstream ss;
    ss << "benchmark" << ++index;
    return ss.str();
}

string formatTime(double t, bool fixedWidth = true) {
    stringstream ss;
    ss << setprecision(2) << fixed;
    if (fixedWidth)
        ss << setw(6);
    if (t >= 1) {
        ss << t << " s";
    } else if (t >= 1e-3) {
        ss << 1e3 * t << " ms";
    } else if (t >= 1e-6) {
        ss << 1e6 * t << " μs";
    } else {
        ss << 1e9 * t << " ns";
    }
    return ss.str();
}

void benchmark(void (*f)(), string name = next(), int warmup = 20, int iters = 100) {
    for (int i = 0; i < warmup; i++) {
        f();
    }
    using namespace chrono;
    duration<double> times[iters];
    for (int i = 0; i < iters; i++) {
        auto start = high_resolution_clock::now();
        f();
        auto end = high_resolution_clock::now();
        times[i] = end - start;
    }
    double mean = 0;
    for (int i = 0; i < iters; i++) {
        mean += times[i].count();
    }
    mean /= iters;
    double err = 0;
    for (int i = 0; i < iters; i++) {
        err += (mean - times[i].count()) * (mean - times[i].count());
    }
    err = sqrt(err / (iters - 1));
    cout << name << ": " << formatTime(mean) << " ± " << formatTime(err, false) << endl;
}

void test1sort() {
    copyList(list1);
    DoNotOptimize(nubSort());
}

void test2sort() {
    copyList(list2);
    DoNotOptimize(nubSort());
}

void test3sort() {
    copyList(list3);
    DoNotOptimize(nubSort());
}

void test4sort() {
    copyList(list4);
    DoNotOptimize(nubSort());
}

void test5sort() {
    copyList(list5);
    DoNotOptimize(nubSort());
}

void test6sort() {
    copyList(list6);
    DoNotOptimize(nubSort());
}

void test1tree() {
    copyList(list1);
    DoNotOptimize(nubTree());
}

void test2tree() {
    copyList(list2);
    DoNotOptimize(nubTree());
}

void test3tree() {
    copyList(list3);
    DoNotOptimize(nubTree());
}

void test4tree() {
    copyList(list4);
    DoNotOptimize(nubTree());
}

void test5tree() {
    copyList(list5);
    DoNotOptimize(nubTree());
}

void test6tree() {
    copyList(list6);
    DoNotOptimize(nubTree());
}

void test1hash() {
    copyList(list1);
    DoNotOptimize(nubHash());
}

void test2hash() {
    copyList(list2);
    DoNotOptimize(nubHash());
}

void test3hash() {
    copyList(list3);
    DoNotOptimize(nubHash());
}

void test4hash() {
    copyList(list4);
    DoNotOptimize(nubHash());
}

void test5hash() {
    copyList(list5);
    DoNotOptimize(nubHash());
}

void test6hash() {
    copyList(list6);
    DoNotOptimize(nubHash());
}

int main(int argc, char** argv) {
    for (int i = 0; i < N / 2; i++) {
        list1[i] = N / 2 - i;
        list1[i + N / 2] = N / 2 - i;
    }
    for (int i = 0; i < N; i++) {
        list2[i] = i;
        list3[i] = i % 100;
        list4[i] = i % 10000;
        list5[i] = i % 100000;
        list6[i] = (((i * i - i * 377) ^ 721 + 127 * i) % 1000 + 1000) % 1000;
    }
    benchmark(test1sort, "list1/nubSort");
    benchmark(test1tree, "list1/nubTree");
    benchmark(test1hash, "list1/nubHash");
    cout << "----------------------------------" << endl;
    benchmark(test2sort, "list2/nubSort");
    benchmark(test2tree, "list2/nubTree");
    benchmark(test2hash, "list2/nubHash");
    cout << "----------------------------------" << endl;
    benchmark(test3sort, "list3/nubSort");
    benchmark(test3tree, "list3/nubTree");
    benchmark(test3hash, "list3/nubHash");
    cout << "----------------------------------" << endl;
    benchmark(test4sort, "list4/nubSort");
    benchmark(test4tree, "list4/nubTree");
    benchmark(test4hash, "list4/nubHash");
    cout << "----------------------------------" << endl;
    benchmark(test5sort, "list5/nubSort");
    benchmark(test5tree, "list5/nubTree");
    benchmark(test5hash, "list5/nubHash");
    cout << "----------------------------------" << endl;
    benchmark(test6sort, "list6/nubSort");
    benchmark(test6tree, "list6/nubTree");
    benchmark(test6hash, "list6/nubHash");
}
