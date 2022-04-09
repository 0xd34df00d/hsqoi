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
import Data.Proxy

type VarEncoder s = A.STUArray s Int Word8 -> Int -> Maybe (ST s Int)

-- `isBounded n b` checks that -b <= n < b
isBounded :: Word8 -> Word8 -> Bool
isBounded d h = d + h < 2 * h
{-# INLINE isBounded #-}

encodeDiff8 :: Word8 -> Word8 -> Word8 -> Word8 -> VarEncoder s
encodeDiff8 dr dg db 0 out outPos
  | (`isBounded` 2) `all` [dr, dg, db] = let byte = 0b01000000 .|. ((dr + 2) .<<. 4)
                                                               .|. ((dg + 2) .<<. 2)
                                                               .|.  (db + 2)
                                          in Just $ A.unsafeWrite out outPos byte $> 1
encodeDiff8 _ _ _ _ _ _  = Nothing
{-# INLINE encodeDiff8 #-}

encodeDiffLuma :: Word8 -> Word8 -> Word8 -> Word8 -> VarEncoder s
encodeDiffLuma dr dg db 0 out outPos
  | dg `isBounded` 32
  , uncurry isBounded `all` [(dr_dg, 8), (db_dg, 8)] = let b1 = 0b10000000 .|. (dg + 32)
                                                           b2 = (dr_dg + 8) .<<. 4
                                                            .|. (db_dg + 8)
                                                        in Just $ do A.unsafeWrite out outPos       b1
                                                                     A.unsafeWrite out (outPos + 1) b2
                                                                     pure 2
  where
    dr_dg = dr - dg
    db_dg = db - dg
encodeDiffLuma _ _ _ _ _ _  = Nothing
{-# INLINE encodeDiffLuma #-}

encodeIndex :: Eq pixel => pixel -> Word8 -> pixel -> VarEncoder s
encodeIndex px hash runningPx out outPos
  | px == runningPx = Just $ A.unsafeWrite out outPos hash $> 1
  | otherwise = Nothing
{-# INLINE encodeIndex #-}

encodeColor :: Pixel pixel => pixel -> pixel -> VarEncoder s
encodeColor px1 px0 out outPos = Just $ do
  A.unsafeWrite out outPos $ 0b11111110 .|. fromIntegral ha
  A.unsafeWrite out (outPos + 1) r1
  A.unsafeWrite out (outPos + 2) g1
  A.unsafeWrite out (outPos + 3) b1
  when (ha == 1) $ A.unsafeWrite out (outPos + 4) a1
  pure (4 + ha)
  where
    (r1, g1, b1, a1) = toRGBA px1
    (_,  _,  _,  a0) = toRGBA px0
    ha = fromEnum $ a1 /= a0
{-# INLINE encodeColor #-}

encodeRun :: Int -> A.STUArray s Int Word8 -> Int -> ST s Int
encodeRun 0 _ outPos = pure outPos
encodeRun runLen out outPos = A.unsafeWrite out outPos (0b11000000 .|. fromIntegral (runLen - 1)) $> outPos + 1
{-# INLINE encodeRun #-}

maxResultSize :: Header -> (Int, BS.ByteString)
maxResultSize h@Header { .. } = (maxLen, headerBS)
  where
    headerBS = BSL.toStrict $ encode h
    maxLen = BS.length headerBS
           + fromIntegral (hChannels + 1) * fromIntegral (hWidth * hHeight)
           + 8 -- end padding

encodeIntoArray :: forall pixel s. (Pixel pixel, Show pixel)
                => Proxy pixel
                -> Int
                -> BS.ByteString
                -> Int
                -> A.STUArray s Int Word8
                -> ST s Int
encodeIntoArray proxy headerLen inBytes startPos result = do
  running <- A.newArray @(A.STUArray s) (0, 63 :: Int) (fromRGBA @pixel 0 0 0 255)

  let step inPos runLen prevPx outPos
        | inPos + diff <= inLen
        , readPixel inBytes inPos == prevPx =
          if runLen /= maxRunLen - 1
             then step (inPos + diff) (runLen + 1) prevPx outPos
             else encodeRun maxRunLen result outPos >>= step (inPos + diff) 0 prevPx
        | inPos + diff <= inLen = do
          let (r0, g0, b0, a0) = toRGBA prevPx
          let px = readPixel inBytes inPos
          let (r1, g1, b1, a1) = toRGBA px
          let (dr, dg, db, da) = (r1 - r0, g1 - g0, b1 - b0, a1 - a0)
          outPos' <- encodeRun runLen result outPos

          let hash = pixelHash px
          pxDiff <- case encodeDiff8 dr dg db da result outPos' of
                         Just act -> act
                         _ -> do runningPx <- A.unsafeRead running hash
                                 fromJust $ encodeIndex px (fromIntegral hash) runningPx result outPos'
                                        <|> encodeDiffLuma dr dg db da result outPos'
                                        <|> encodeColor px prevPx result outPos'

          A.unsafeWrite running hash px

          step (inPos + diff) 0 px (outPos' + pxDiff)
        | otherwise = encodeRun runLen result outPos

  step startPos 0 (fromRGBA 0 0 0 255) headerLen
  where
    inLen = BS.length inBytes
    diff = channelCount proxy
{-# INLINE encodeIntoArray #-}

encodeRaw :: Header -> BS.ByteString -> Int -> A.UArray Int Word8
encodeRaw header inBytes' startPos = A.runSTUArray $ do
  result <- A.unsafeNewArray_ (0, maxLen - 1)
  forM_ [0 .. headerLen - 1] $ \i -> A.unsafeWrite result i (headerBS ! i)
  final <- if hChannels header == 3
              then encodeIntoArray @Pixel3 Proxy headerLen inBytes startPos result
              else encodeIntoArray @Pixel4 Proxy headerLen inBytes startPos result
  forM_ [0..6] $ \i -> A.unsafeWrite result (final + i) 0
  A.unsafeWrite result (final + 7) 1
  pure $ unsafeShrink result (final + 8)
  where
    inBytes = unoffsetBS inBytes'
    (maxLen, headerBS) = maxResultSize header
    headerLen = BS.length headerBS
