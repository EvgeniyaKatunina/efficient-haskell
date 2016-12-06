import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @author Moklev Vyacheslav
 */
public class Benchmark {
    private static final int WARMUP = 20;
    private static final int ITERS = 100;

    public static <T> void benchmark(Consumer<List<T>> f, List<T> list, String name) {
        for (int i = 0; i < WARMUP; i++) {
            f.accept(list);
        }
        long[] times = new long[ITERS];
        for (int i = 0; i < ITERS; i++) {
            List<T> copyList = new ArrayList<T>(list);
            System.gc();
            long time = System.nanoTime();
            f.accept(copyList);
            times[i] = System.nanoTime() - time;
        }
        double mean = Arrays.stream(times).average().orElse(0);
        double err = Math.sqrt(Arrays.stream(times)
                .mapToDouble(x -> (x - mean) * (x - mean))
                .sum() / (ITERS - 1));
        System.out.println(name + ":\t " + formatTime(mean / 1_000_000_000, true) + " ± " + formatTime(err / 1_000_000_000, false));
    }

    private static String formatTime(double t, boolean fixedWidth) {
        String fmt = fixedWidth ? "%6.2f" : "%.2f";
        if (t >= 1) {
            return String.format(fmt, t) + " s";
        } else if (t >= 1e-3) {
            return String.format(fmt, 1e3 * t) + " ms";
        } else if (t >= 1e-6) {
            return String.format(fmt, 1e6 * t) + " µs";
        } else {
            return String.format(fmt, 1e9 * t) + " ns";
        }
    }
}
