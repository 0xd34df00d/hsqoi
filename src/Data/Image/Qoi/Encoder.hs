{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -O2 -fllvm #-}

module Data.Image.Qoi.Encoder
( encodeRaw
) where

import Prelude hiding (Maybe(..))

import qualified Data.Array.Base as A
import qualified Data.Array.ST as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Binary
import Data.Bits
import Data.Functor
import Data.Maybe

import Data.Image.Qoi.Pixel
import Data.Image.Qoi.Util
import Data.Image.Qoi.Format

readPixel3 :: BS.ByteString -> Int -> Pixel3
readPixel3 str pos = Pixel3 (str ! pos)
                            (str ! pos + 1)
                            (str ! pos + 2)
{-# INLINE readPixel3 #-}

type VarEncoder s = A.STUArray s Int Word8 -> Int -> Maybe (ST s Int)

-- `isBounded n b` checks that -b <= n < b
isBounded :: Word8 -> Word8 -> Bool
isBounded d h = d + h < 2 * h
{-# INLINE isBounded #-}

encodeDiff8 :: Word8 -> Word8 -> Word8 -> VarEncoder s
encodeDiff8 dr dg db out outPos
  | (`isBounded` 2) `all` [dr, dg, db] = let byte = 0b10000000 .|. ((dr + 2) .<<. 4)
                                                               .|. ((dg + 2) .<<. 2)
                                                               .|.  (db + 2)
                                          in Just $ A.unsafeWrite out outPos byte $> 1
  | otherwise = Nothing
{-# INLINE encodeDiff8 #-}

encodeDiff16 :: Word8 -> Word8 -> Word8 -> VarEncoder s
encodeDiff16 dr dg db out outPos
  | uncurry isBounded `all` [(dr, 16), (dg, 8), (db, 8)] = let b1 = 0b11000000 .|. (dr + 16)
                                                               b2 = (dg + 8) .<<. 4
                                                                .|. (db + 8)
                                                            in Just $ do A.unsafeWrite out outPos       b1
                                                                         A.unsafeWrite out (outPos + 1) b2
                                                                         pure 2
  | otherwise = Nothing
{-# INLINE encodeDiff16 #-}

encodeDiff24 :: Word8 -> Word8 -> Word8 -> Word8 -> VarEncoder s
encodeDiff24 dr dg db da out outPos
  | (`isBounded` 16) `all` [dr, dg, db, da] = let bytes :: Word32
                                                  bytes = (0b11100000 .<<. 16)
                                                      .|. fromIntegral (dr + 16) .<<. 15
                                                      .|. fromIntegral (dg + 16) .<<. 10
                                                      .|. fromIntegral (db + 16) .<<. 5
                                                      .|. fromIntegral (da + 16)
                                               in Just $ do A.unsafeWrite out outPos       (fromIntegral $ bytes .>>. 16)
                                                            A.unsafeWrite out (outPos + 1) (fromIntegral $ bytes .>>. 8)
                                                            A.unsafeWrite out (outPos + 2) (fromIntegral   bytes)
                                                            pure 3
  | otherwise = Nothing
{-# INLINE encodeDiff24 #-}

encodeIndex :: Pixel3 -> Word8 -> Pixel3 -> VarEncoder s
encodeIndex px hash runningPx out outPos
  | px == runningPx = Just $ A.unsafeWrite out outPos hash $> 1
  | otherwise = Nothing
{-# INLINE encodeIndex #-}

encodeColor :: Pixel3 -> Pixel3 -> VarEncoder s
encodeColor (Pixel3 r1 g1 b1) (Pixel3 r0 g0 b0) out outPos = Just $ do
  A.unsafeWrite out outPos bh
  when (hr == 1) $ A.unsafeWrite out (outPos + 1) r1
  when (hg == 1) $ A.unsafeWrite out (outPos + 1 + hr) g1
  when (hb == 1) $ A.unsafeWrite out (outPos + 1 + hr + hg) b1
  pure (1 + hr + hg + hb)
  where
    hr = fromEnum $ r1 /= r0
    hg = fromEnum $ g1 /= g0
    hb = fromEnum $ b1 /= b0
    bh = 0b11110000 .|. (fromIntegral hr .<<. 3)
                    .|. (fromIntegral hg .<<. 2)
                    .|. (fromIntegral hb .<<. 1)
{-# INLINE encodeColor #-}

maxRunLen :: Int
maxRunLen = 8224

encodeRun :: Int -> A.STUArray s Int Word8 -> Int -> ST s Int
encodeRun runLen out outPos
  | runLen == 0 = pure 0
  | runLen <= 32 = A.unsafeWrite out outPos (0b01000000 .|. fromIntegral (runLen - 1)) $> 1
  | runLen <= 8224 = do let runLen' = runLen - 33
                        A.unsafeWrite out outPos       (0b01100000 .|. fromIntegral (runLen' .>>. 8))
                        A.unsafeWrite out (outPos + 1) (fromIntegral runLen')
                        pure 2
  | otherwise = pure 0
{-# INLINE encodeRun #-}

maxResultSize :: Header -> (Int, BS.ByteString)
maxResultSize h@Header { .. } = (maxLen, headerBS)
  where
    headerBS = BSL.toStrict $ encode h
    maxLen = BS.length headerBS
           + fromIntegral (hChannels + 1) * fromIntegral (hWidth * hHeight)
           + 4 -- end padding

encodeRaw :: Header -> BS.ByteString -> Int -> A.UArray Int Word8
encodeRaw header inBytes startPos = A.runSTUArray $ do
  (result :: A.STUArray s Int Word8) <- A.unsafeNewArray_ (0, maxLen - 1)

  forM_ [0 .. headerLen - 1] $ \i -> A.unsafeWrite result i (headerBS ! i)

  running <- A.newArray @(A.STUArray s) (0, 63 :: Int) initPixel

  let step inPos outPos px@(Pixel3 r1 g1 b1) prevPx@(Pixel3 r0 g0 b0) = do
        let (dr, dg, db) = (r1 - r0, g1 - g0, b1 - b0)

        let hash = pixelHash px
        pxDiff <- case encodeDiff8 dr dg db result outPos of
                       Just act -> act
                       _ -> do runningPx <- A.unsafeRead running hash
                               fromJust $ encodeIndex px (fromIntegral hash) runningPx result outPos
                                      <|> encodeDiff16 dr dg db result outPos
                                      <|> encodeDiff24 dr dg db 0 result outPos
                                      <|> encodeColor px prevPx result outPos

        A.unsafeWrite running hash px
        let tryRun pos runLen
              | pos + 3 <= inLen = do
                let thisPixel = readPixel3 inBytes pos
                if thisPixel /= px || runLen >= maxRunLen
                   then (pos, runLen, thisPixel)
                   else tryRun (pos + 3) (runLen + 1)
              | otherwise = (pos + 3, runLen, px)

        let (afterRun, runLen, nextPix) = tryRun (inPos + 3) 0
        runDiff <- encodeRun runLen result (outPos + pxDiff)

        if afterRun <= inLen
           then step afterRun (outPos + pxDiff + runDiff) nextPix px
           else pure outPos
  final <- step startPos headerLen (readPixel3 inBytes startPos) initPixel
  forM_ [0..3] $ \i -> A.unsafeWrite result (final + i) 0

  pure $ unsafeShrink result (final + 4)
  where
    inLen = BS.length inBytes
    (maxLen, headerBS) = maxResultSize header
    headerLen = BS.length headerBS
