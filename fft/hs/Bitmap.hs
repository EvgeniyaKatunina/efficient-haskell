{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Bitmap where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.Base      as AB
import           Data.Array.IO
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import           GHC.Word
import           System.IO

data Bitmap s = Bitmap
    { width  :: !Int
    , height :: !Int
    , pixels :: !(STUArray s Int Double)
    }

newBitmap :: Int -> Int -> ST s (Bitmap s)
newBitmap w h = do
    arr <- newArray (0, w * h) 0.0
    return $ Bitmap w h arr

pixelIndex :: Bitmap s -> Int -> Int -> Int
pixelIndex (Bitmap w _ _) x y = x + y * w

modifyPixel :: Bitmap s -> (Double -> Double) -> Int -> Int -> ST s ()
modifyPixel bm@(Bitmap _ _ px) fn x y = do
    let idx = pixelIndex bm x y
    ov <- readArray px idx
    writeArray px idx $ fn ov

readPixel :: Bitmap s -> Int -> Int -> ST s Double
readPixel bm@(Bitmap _ _ px) x y = readArray px $ pixelIndex bm x y

writeHeader :: Handle -> Bitmap RealWorld -> IO ()
writeHeader hdl (Bitmap w h _) =
    B.hPut hdl $ runPut $ do
        putWord16le 0x4d42
        putWord32le $ fromIntegral $ 14 + 40 + w * h * 3
        putWord32le 0
        putWord32le $ 14 + 40
        putWord32le 40
        putWord32le $ fromIntegral w
        putWord32le $ fromIntegral h
        putWord16le 1
        putWord16le 24
        forM_ [1..6::Int] $ const $ putWord32le 0

bitmapTrunc :: Integral a => Double -> a
bitmapTrunc !d = floor $ 255 * max 0.0 (min 1.0 d)

writeBody :: Handle -> Bitmap RealWorld -> IO ()
writeBody hdl (Bitmap w h px) = do
    arr <- newArray_ (0, w * h * 3) :: IO (IOUArray Int Word8)
    oldArray <- stToIO $ freeze px :: IO (UArray Int Double)
    forM_ [0..w*h-1] $ \i -> do
        let pv = bitmapTrunc $ oldArray AB.! i
        writeArray arr (i * 3) pv
        writeArray arr (i * 3 + 1) pv
        writeArray arr (i * 3 + 2) pv
    hPutArray hdl arr (w * h * 3)

writeAll :: Handle -> Bitmap RealWorld -> IO ()
writeAll hdl bm = do
    writeHeader hdl bm
    writeBody hdl bm
