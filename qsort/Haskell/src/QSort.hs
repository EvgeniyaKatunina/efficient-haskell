{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}

module QSort where
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef
import Control.Monad.Random
import Control.Monad.Trans
import System.IO.Unsafe

quickSort :: forall a . Ord a => [a] -> [a]
quickSort items = runST $ do
    let total = length items
    arr <- newListArray (1, total) items :: ST s (STArray s Int a)

    let qsSwap arr i j = do
        v <- readArray arr i
        readArray arr j >>= writeArray arr i
        writeArray arr j v

    let qsPartition arr p r = do
        i <- newSTRef (p - 1)
        let m = unsafePerformIO $ randomRIO (p, r)
        s <- readArray arr m

        forM_ [p .. pred r] $ \j -> do
            jj <- readArray arr j
            when (jj <= s) $ do
                modifySTRef i (+1)
                readSTRef i >>= \ii -> qsSwap arr ii j
        ii <- readSTRef i
        qsSwap arr (ii + 1) r
        return (ii + 1)

    let quickSort' arr p r = when (p < r) $ do
        q <- qsPartition arr p r
        quickSort' arr p (q - 1)
        quickSort' arr (q + 1) r

    quickSort' arr 1 total
    getElems arr