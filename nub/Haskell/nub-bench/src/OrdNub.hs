module OrdNub where

import qualified Data.Set as Set
import           Data.List (sort, group)

ordNub :: Ord a => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

ordNub2 :: Ord a => [a] -> [a]
ordNub2 = map head . group . sort

ordNub3 :: Ord a => [a] -> [a]
ordNub3 = collapse . sort 
    where collapse [] = []
          collapse [x] = [x]
          collapse (x:y:xs) = if x == y then 
                                  collapse (y:xs)
                              else
                                  x : collapse (y:xs)