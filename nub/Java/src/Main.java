import javafx.util.Pair;
import nub.NubImpl;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * @author Moklev Vyacheslav
 */
public class Main {
    private static final int N = 200_000;

    public static void main(String[] args) {
        List<Integer> list1 = new ArrayList<>();
        List<Integer> list2 = new ArrayList<>();
        List<Integer> list3 = new ArrayList<>();
        List<Integer> list4 = new ArrayList<>();
        List<Integer> list5 = new ArrayList<>();
        List<Integer> list6 = new ArrayList<>();
        for (int i = 0; i < N / 2; i++) {
            list1.add(i);
        }
        for (int i = 0; i < N / 2; i++) {
            list1.add(i);
        }
        for (int i = 0; i < N; i++) {
            list2.add(i);
            list3.add(i % 100);
            list4.add(i % 10_000);
            list5.add(i % 100_000);
            list6.add((((i * i - i * 377) ^ 721 + 127 * i) % 1000 + 1000) % 1000);
        }
        List<Pair<List<Integer>, String>> lists = new ArrayList<>();
        lists.add(new Pair<>(list1, "list1"));
        lists.add(new Pair<>(list2, "list2"));
        lists.add(new Pair<>(list3, "list3"));
        lists.add(new Pair<>(list4, "list4"));
        lists.add(new Pair<>(list5, "list5"));
        lists.add(new Pair<>(list6, "list6"));
        List<Pair<Consumer<List>, String>> conses = new ArrayList<>();
        conses.add(new Pair<>(NubImpl::nubSort, "nubSort"));
        conses.add(new Pair<>(NubImpl::nubTree, "nubTree"));
        conses.add(new Pair<>(NubImpl::nubTree2, "nubTree2"));
        conses.add(new Pair<>(NubImpl::nubHash, "nubHash"));
        conses.add(new Pair<>(NubImpl::nubHash2, "nubHash2"));
        conses.add(new Pair<>(NubImpl::nubStream, "nubStream"));
        for (Pair<List<Integer>, String> list: lists) {
            for (Pair<Consumer<List>, String> cons: conses) {
                Benchmark.benchmark(x -> cons.getKey().accept(x), list.getKey(), list.getValue() + "/" + cons.getValue());
            }
            System.out.println("----------------------------------");
        }
    }
}
