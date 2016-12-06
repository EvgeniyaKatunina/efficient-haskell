module HashNub where

import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable)

hashNub :: (Eq a, Hashable a) => [a] -> [a]
hashNub l = go HashSet.empty l
  where
    go _ []     = []
    go s (x:xs) =
      if x `HashSet.member` s
      then go s xs
      else x : go (HashSet.insert x s) xs