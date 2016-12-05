{-# LANGUAGE ScopedTypeVariables #-}

module StuNub where

import           Data.Foldable (for_, forM_, foldlM)
import           Control.Monad (when, liftM)
import           Data.List (sort)
import qualified Data.Set as Set
import           UnboxArray (Unbox(..))
import           Control.Monad.ST (ST, runST)
import           Data.STRef (newSTRef, readSTRef, modifySTRef)
import           Data.Array.ST (STUArray, MArray, Ix, newListArray, readArray, writeArray, getElems)

stuNubSort :: (Ord a, Unbox a) => [a] -> [a]
stuNubSort [] = []
stuNubSort (list :: [a]) = runST $ do
    let lastIndex = length list - 1
    arr <- newListUArray (0, lastIndex) (sort list) :: ST s (STUArray s Int a)
    posRef <- newSTRef 1
    for_ [1..lastIndex] $ \i -> do
        elem <- readUArray arr i
        prev <- readUArray arr (i - 1)
        when (elem /= prev) $ do
            pos <- readSTRef posRef
            writeUArray arr pos elem
            modifySTRef posRef (+1)
    pos <- readSTRef posRef
    res <- getUElems arr
    return $ take pos res

stuNub :: (Ord a, Unbox a) => [a] -> [a]
stuNub (list :: [a]) = runST $ do
    let listSize = length list
    arr <- newListUArray (1, listSize) list :: ST s (STUArray s Int a)
    posRef <- newSTRef 1
    setRef <- newSTRef Set.empty
    forM_ [1..listSize] $ \i -> do
        elm <- readUArray arr i
        set <- readSTRef setRef
        when (elm `Set.notMember` set) $ do
            pos <- readSTRef posRef
            writeUArray arr pos elm
            modifySTRef posRef (+1)
            modifySTRef setRef $ Set.insert elm
    pos <- readSTRef posRef
    res <- getUElems arr
    return $ take (pos - 1) res