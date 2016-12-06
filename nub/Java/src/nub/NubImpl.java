package nub;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Moklev Vyacheslav
 */
public class NubImpl {
    public static <T extends Comparable<T>> List<T> nubSort(List<T> list) {
        if (list.isEmpty())
            return Collections.emptyList();
        Collections.sort(list);
        List<T> result = new ArrayList<>();
        result.add(list.get(0));
        for (int i = 1; i < list.size(); i++) {
            if (!list.get(i).equals(list.get(i - 1))) {
                result.add(list.get(i));
            }
        }
        return result;
    }

    public static <T extends Comparable<T>> List<T> nubTree(List<T> list) {
        Set<T> set = new TreeSet<>();
        List<T> result = new ArrayList<>();
        list.stream().filter(item -> !set.contains(item)).forEach(item -> {
            result.add(item);
            set.add(item);
        });
        return result;
    }

    public static <T extends Comparable<T>> List<T> nubTree2(List<T> list) {
        return new ArrayList<>(new TreeSet<>(list));
    }

    public static <T> List<T> nubHash(List<T> list) {
        Set<T> set = new HashSet<>();
        List<T> result = new ArrayList<>();
        list.stream().filter(item -> !set.contains(item)).forEach(item -> {
            result.add(item);
            set.add(item);
        });
        return result;
    }

    public static <T> List<T> nubHash2(List<T> list) {
        return new ArrayList<>(new HashSet<>(list));
    }

    public static <T> List<T> nubStream(List<T> list) {
        return list.stream()
                .distinct()
                .collect(Collectors.toList());
    }
}
