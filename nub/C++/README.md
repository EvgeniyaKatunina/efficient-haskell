# C++

## Реализации
* nubSort
  * `std::sort` + `std::unique`
* nubTree
  * Проход по массиву + `map`
* nubHash
  * Проход по массиву + `unordered_map`

## Замеры времени 
```
list1/nubSort:  30.90 ms ± 3.58 ms
list1/nubTree:  80.75 ms ± 5.19 ms
list1/nubHash:  88.29 ms ± 8.49 ms
----------------------------------
list2/nubSort:   3.38 ms ± 1.91 ms
list2/nubTree: 150.35 ms ± 9.66 ms
list2/nubHash: 177.41 ms ± 9.91 ms
----------------------------------
list3/nubSort:  10.85 ms ± 3.54 ms
list3/nubTree:   2.60 ms ± 1.61 ms
list3/nubHash:   4.76 ms ± 2.54 ms
----------------------------------
list4/nubSort:  13.80 ms ± 3.72 ms
list4/nubTree:  24.52 ms ± 2.18 ms
list4/nubHash:  13.05 ms ± 3.11 ms
----------------------------------
list5/nubSort:  30.45 ms ± 4.11 ms
list5/nubTree:  84.94 ms ± 6.46 ms
list5/nubHash:  92.04 ms ± 7.16 ms
----------------------------------
list6/nubSort:  16.31 ms ± 2.99 ms
list6/nubTree:  18.63 ms ± 3.49 ms
list6/nubHash:   9.27 ms ± 3.58 ms
```