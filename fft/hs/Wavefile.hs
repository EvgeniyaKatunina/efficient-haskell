module Wavefile where

import           Control.Monad
import           Data.Array
import           Data.Array.IO
import           Data.Array.Unboxed as U
import qualified Data.ByteString    as B
import           Data.Foldable
import           GHC.Int
import           GHC.Word

data WaveFile = WaveFile
    { frequency :: Int
    , channels  :: Array Int (UArray Int Int16)
    }

getInt32 :: B.ByteString -> Int -> Int
getInt32 str idx = fromIntegral $ foldr' (\b a -> a * (256 :: Int32) + b) 0 $ map (fromIntegral . B.index str . fromIntegral) [idx..idx+3]

getInt16 :: B.ByteString -> Int -> Int
getInt16 str idx = fromIntegral $ foldr' (\b a -> a * (256 :: Int16) + b) 0 $ map (fromIntegral . B.index str . fromIntegral) [idx..idx+1]

readWAV :: FilePath -> IO WaveFile
readWAV fp = do
    bytes <- B.readFile fp
    let chans = getInt16 bytes 22
    let sampleRate = getInt32 bytes 24
    let numSamples = getInt32 bytes 40 `div` 4

    --print chans
    --print sampleRate
    --print numSamples

    chArrs <- newArray_ (1, chans) :: IO (IOArray Int (IOUArray Int Int16))
    forM_ [1..chans] $ \i -> do
        cca <- newArray_ (0, numSamples - 1) :: IO (IOUArray Int Int16)
        writeArray chArrs i cca

    let prefixSize = 4 * 5 + 2 + 2 + 4 + 4 + 2 + 2 + 4 + 4
    forM_ [0..numSamples-1] $ \i ->
        forM_ [1..chans] $ \j -> do
            ta <- readArray chArrs j
            writeArray ta i (fromIntegral ((fromIntegral (B.index bytes $ fromIntegral $ prefixSize + (i * chans * 2 + j * 2 - 2)) :: Word16) + 256 * fromIntegral (B.index bytes $ fromIntegral $ prefixSize + (i * chans * 2 + j * 2 - 1))) :: Int16)


    let defa = U.array (0, 0) [(0, 0)] :: UArray Int Int16
    na <- newArray (1, chans) defa :: IO (IOArray Int (UArray Int Int16))
    forM_ [1..chans] $ \i -> do
        tmp <- readArray chArrs i
        fres <- freeze tmp
        writeArray na i fres
    WaveFile sampleRate <$> freeze na
