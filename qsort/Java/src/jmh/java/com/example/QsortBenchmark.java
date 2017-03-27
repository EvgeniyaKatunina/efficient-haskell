package com.example;

import org.openjdk.jmh.annotations.*;

import java.util.Arrays;
import java.util.Random;

/**
 * Created by jetbrains on 3/28/2017.
 */
@State(Scope.Thread)
public class QsortBenchmark {
    int[] ints1000 = new int[1000];

    int[] ints5000 = new int[5000];

    Qsort q = new Qsort();

    @Setup(Level.Invocation)
    public void setup() {
        Random r = new Random();
        for (int i = 0; i < 1000; ++i) {
            ints1000[i] = r.nextInt();
        }

        for (int i = 0; i < 5000; ++i) {
            ints5000[i] = r.nextInt();
        }
    }

    @Benchmark
    public int[] benchmarkStd1000() {
        Arrays.sort(ints1000);
        return ints1000;
    }

    @Benchmark
    public int[] benchmarkStd5000() {
        Arrays.sort(ints5000);
        return ints1000;
    }

    @Benchmark
    public int[] benchmarkQsort1000() {
        q.quickSort(ints1000, 0, 999);
        return ints1000;
    }

    @Benchmark
    public int[] benchmarkQsort5000() {
        q.quickSort(ints5000, 0, 4999);
        return ints5000;
    }
}
