{-# LANGUAGE FlexibleContexts #-}

module UnboxArray where

import           Control.Monad.ST (ST, runST)
import           Data.Array.Unboxed (UArray)
import           Data.Array.IArray (IArray)
import           Data.Array.ST (STUArray, MArray, Ix, newListArray, readArray, writeArray, getElems)

class IArray UArray e => Unbox e where
    newListUArray :: Ix i => (i, i) -> [e] -> ST s (STUArray s i e)
    readUArray    :: Ix i => STUArray s i e -> i -> ST s e
    writeUArray   :: Ix i => STUArray s i e -> i -> e -> ST s ()
    getUElems     :: Ix i => STUArray s i e -> ST s [e]

instance Unbox Int where
    newListUArray = newListArray
    readUArray    = readArray
    writeUArray   = writeArray
    getUElems     = getElems