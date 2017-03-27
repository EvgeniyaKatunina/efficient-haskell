module Sorters where
import SearchTree

qsortNaive :: Ord a => [a] -> [a]
qsortNaive []     = []
qsortNaive (p:xs) = qsortNaive lesser ++ [p] ++ (qsortNaive greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs