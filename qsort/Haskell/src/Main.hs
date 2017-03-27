module Main where
import Criterion.Main
import System.Random
import QSort
import Sorters
import Data.List

ascending :: Int -> [Int]
ascending n = [1..n]

descending :: Int -> [Int]
descending n = [n, n-1 .. 1]

rand :: Int -> [Int]
rand n = take n $ randomRs (0, 10^9) (mkStdGen 42)

blocks :: Int -> [Int]
blocks n = concat $ replicate (n `div` 100) [1..100]

sortGroup :: [(String, [Int])] -> (String, [Int] -> [Int]) -> Benchmark
sortGroup cases (name, sorter) = bgroup name $ map (\(caseName, caseData) -> bench caseName $ nf sorter caseData) cases

myListTypes = [
               ("ascending", ascending),
               ("descending", descending),
               ("rand", rand),
               ("blocks", blocks)
              ]

myNs = [1000, 5000]

mySorters = [
             ("std", sort),
             ("qsort-naive", qsortNaive),
             ("quickSort", quickSort)
            ]

myCases = do (listTypeName, listGenerator) <- myListTypes
             n <- myNs
             return (listTypeName ++ "/" ++ show n, listGenerator n)

myGroups = map (sortGroup myCases) mySorters

main = defaultMain myGroups