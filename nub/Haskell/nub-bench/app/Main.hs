module Main where

import           Criterion.Main
import           Data.List.Unique (sortUniq, unique)
import           OrdNub (ordNub, ordNub2, ordNub3)
import           HashNub (hashNub)
import           StuNub (stuNub, stuNubSort)
import qualified Data.Set as Set

-- @Todo:
--   * IntSet
--   * Discrimination

testList1 :: [Int]
testList1 = concat $ replicate 2 [100000,99999..1]

testList2 :: [Int]
testList2 = [1..200000]

testList3 :: [Int]
testList3 = map (`mod` 100) [1..200000]

testList4 :: [Int]
testList4 = map (`mod` 10000) [1..200000]

testList5 :: [Int]
testList5 = map (`mod` 100000) [1..200000]

main :: IO ()
main = defaultMain $ concat $ map (\list -> 
    [ bgroup (fst list)
        [ bench "ordNub"     $ nf ordNub     $ snd list
        , bench "ordNub2"    $ nf ordNub2    $ snd list
        , bench "ordNub3"    $ nf ordNub3    $ snd list
        , bench "sortUniq"   $ nf sortUniq   $ snd list
        , bench "unique"     $ nf unique     $ snd list
        , bench "hashNub"    $ nf hashNub    $ snd list
        , bench "stuNub"     $ nf stuNub     $ snd list
        , bench "stuNubSort" $ nf stuNubSort $ snd list
        ]
    ]) [  ("DoubleReverse"       , testList1)
        , ("StraightForward"     , testList2)
        , ("100UniquesSorted"    , testList3)
        , ("10000Uniques"        , testList4)
        , ("10^5Uniques2*10^5All", testList5)
        ]
