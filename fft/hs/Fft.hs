{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fft where

import           Bitmap
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.Base     as AB
import           Data.Array.IO
import           Data.Foldable       as F
import           Data.HashMap.Strict as M
import           Data.STRef
import           System.IO
import           Wavefile

type Map k v = HashMap k v

downPowerList :: Int -> [Int]
downPowerList 1 = [1]
downPowerList a = a : downPowerList (a `div` 2)

type TrigTable = Map Int (Map Int (Double, Double))
makeTrigTable :: Int -> TrigTable
makeTrigTable maxLength = M.fromList $ flip fmap (downPowerList maxLength) $ \currentLength ->
    (currentLength, M.fromList $ flip fmap ((*) <$> [0..currentLength] <*> [0..currentLength]) $ \idx ->
        (idx, (sin $ -2 * pi * fromIntegral idx / fromIntegral currentLength, cos $ -2 * pi * fromIntegral idx / fromIntegral currentLength)))


chebyPoly :: Int -> Double -> Double
chebyPoly n x
    | x >= -1 && x <= 1 = cos $ fromIntegral n * acos x
    | otherwise = cosh $ fromIntegral n * acosh x


chebyWin :: Int -> Double -> UArray Int Double
chebyWin maxLength atten = runST $ do
    let tg = 10 ** (atten / 20.0)
    let x0 = cosh $ acosh tg / (fromIntegral maxLength - 1)
    arr <- newArray_ (0, maxLength) :: ST s (STUArray s Int Double)
    let m = (fromIntegral maxLength-1)/2 + 0.5
    maxRef <- newSTRef (0 :: Double)
    forM_ [0..(maxLength `div` 2 + 1)] $ \nn -> do
        let n = fromIntegral nn - m
        let s = sum $ fmap (\x -> cos (2 * pi * n * fromIntegral x / fromIntegral maxLength) * chebyPoly (maxLength - 1) (x0 * cos (pi * fromIntegral x / fromIntegral maxLength))) [1..floor m :: Int]
        let cv = tg + 2 * s
        writeArray arr nn cv
        writeArray arr (maxLength - nn - 1) cv
        modifySTRef maxRef (max cv)
    writeArray arr maxLength 0
    maxVal <- readSTRef maxRef
    forM_ [0..maxLength] $ \nn -> do
        v <- readArray arr nn
        writeArray arr nn $ v / maxVal
    freeze arr

type WindowTable = Map Int (UArray Int Double)
makeWindowTables :: Int -> WindowTable
makeWindowTables maxLength = M.fromList $ flip fmap (Prelude.filter (>2) $ downPowerList maxLength) $ \currentLength ->
    (currentLength, chebyWin currentLength 50.0)

dft2 :: (IArray a Double) => TrigTable -> WindowTable -> a Int Double -> Int -> Int -> Int -> STUArray s Int Double -> STUArray s Int Double -> Int -> ST s ()
dft2 !trig !wnd !arr !pos !len !step !rvr !rvi !off = do
    let lenList = [0..len-1]
    let outerLoop !i = when (i < len) $ do
            let (!cnrV, !cniV) = F.foldr' (\j (!cnr, !cni) -> let
                        coef = (arr AB.! (pos + j * step)) * ((wnd M.! (len * step)) AB.! (j * step + off))
                        trig_pair = (trig M.! len) M.! (i * j)
                    in (cnr + coef * snd trig_pair, cni + coef * fst trig_pair)) (0, 0) lenList
            writeArray rvr (i + 1) cnrV
            writeArray rvi (i + 1) cniV
            outerLoop $ i + 1
    outerLoop 0

dft1 :: (IArray a Double) => TrigTable -> WindowTable -> a Int Double -> Int -> Int -> Int -> STUArray s Int Double -> STUArray s Int Double -> Int -> ST s ()
dft1 !trig !wnd !arr !pos !len !step !rvr !rvi !off = do
    let coef = (arr AB.! pos) * ((wnd M.! step) AB.! off)
    let (!sinv, !cosv) = (trig M.! 1) M.! 0
    let !cnrV = coef * cosv
    let !cniV = coef * sinv
    writeArray rvr 1 cnrV
    writeArray rvi 1 cniV

ifft :: (IArray a Double) => TrigTable -> WindowTable -> a Int Double -> Int -> Int -> Int -> STUArray s Int Double -> STUArray s Int Double -> Int -> ST s ()
ifft !trig !wnd !arr !pos !len !step !str !sti !off =
    if len <= 1 then dft1 trig wnd arr pos len step str sti off
    else do
        ifft trig wnd arr (pos + step) (len `div` 2) (step * 2) str sti (off + step)
        let !lenHalf = len `div` 2
        let ifftFirstLoop !i = when (i <= lenHalf) $ do
                tr <- readArray str i
                ti <- readArray sti i
                writeArray str (i + lenHalf) tr
                writeArray sti (i + lenHalf) ti
                ifftFirstLoop $ i + 1
        ifftFirstLoop 1
        ifft trig wnd arr pos (lenHalf) (step * 2) str sti off
        let ifftSecondLoop !i = when (i <= lenHalf) $ do
                let !ix2 = i + (lenHalf)
                tr <- readArray str i
                ti <- readArray sti i
                let (!ivi, !ivr) = (trig M.! len) M.! (i - 1)
                xdr <- readArray str ix2
                xdi <- readArray sti ix2
                let !ixmr = ivr * xdr - ivi * xdi
                let !ixmi = ivi * xdr + ivr * xdi
                writeArray str i $ tr + ixmr
                writeArray sti i $ ti + ixmi
                writeArray str ix2 $ tr - ixmr
                writeArray sti ix2 $ ti - ixmi
                ifftSecondLoop $ i + 1
        ifftSecondLoop 1

fft :: (IArray a Double) => TrigTable -> WindowTable -> a Int Double -> Int -> Int -> Bool -> STUArray s Int Double -> STUArray s Int Double -> ST s ()
fft !trig !wnd !arr !pos !len !phases !rvr !rvi = do
    ifft trig wnd arr pos len 1 rvr rvi 0
    forM_ [1 .. (len `div` 2)] $ \i -> do
            cr <- readArray rvr i
            ci <- readArray rvi i
            writeArray rvr i $ sqrt $ cr * cr + ci * ci
            when phases $ writeArray rvi i $ atan2 cr ci

subspectrum :: (IArray a Double) => a Int Double -> Int -> Int -> Int -> Int -> Int -> FilePath -> Double -> IO ()
subspectrum arr window start endpos wide tall fn dc = do
    let cramming_factor = fromIntegral (endpos - start) / fromIntegral wide / dc
    bmp <- stToIO $ newBitmap wide tall
    ft <- stToIO $ newArray_ (1, window) :: IO (STUArray RealWorld Int Double)
    ph <- stToIO $ newArray_ (1, window) :: IO (STUArray RealWorld Int Double)
    let trigTable = makeTrigTable window
    let windowTable = makeWindowTables window
    let dcr = round dc
    let bmLoopBody i = when (i <= endpos) $ do
            fft trigTable windowTable arr i window False ft ph
            let bmInnerLoop j = when (j <= window `div` 2) $ do
                    let cpx = ((i - start) * wide) `div` (endpos - start)
                    let cpy = ((j - 1) * tall * 2) `div` window
                    let cf = 0.15 / cramming_factor
                    let mf x = log (x + 1)
                    ftv <- readArray ft j
                    modifyPixel bmp (\p -> p + mf (ftv / cf) * cf) cpx cpy
                    bmInnerLoop $ j + 1
            bmInnerLoop 1
            bmLoopBody $ i + dcr
    stToIO $ bmLoopBody start
    stToIO $ forM_ [0..wide-1] $ \cc -> do
        av <- foldM (\a cr -> min a <$> readPixel bmp cc cr) 1.0 [0..tall-1]
        forM_ [0..tall-1] $ \cr -> modifyPixel bmp (\nv -> max 0 (nv - av) / (1.0 - av)) cc cr
    hdl <- openBinaryFile fn WriteMode
    writeAll hdl bmp
    hClose hdl

main :: IO ()
main = do
    wav <- readWAV "../Toumei Elegy.wav"
    let chd = channels wav AB.! 1
    let chb = AB.bounds chd
    let freq = frequency wav
    ta <- newArray_ chb :: IO (IOUArray Int Double)
    forM_ [fst chb .. snd chb] $ \i -> writeArray ta i $ fromIntegral (chd AB.! i) / 65536.0

    taf <- freeze ta :: IO (UArray Int Double)
    subspectrum taf 1024 (freq * 20) (freq * 30) 2200 512 "../spectrum_hs.bmp" 25

