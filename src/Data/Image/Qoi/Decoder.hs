{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -O2 -fllvm #-}

module Data.Image.Qoi.Decoder
( decodeQoi
, DecodeError(..)
, SomePixels(..)
) where

import qualified Data.Array.Base as A
import qualified Data.Array.ST as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Bits

import Data.Image.Qoi.Format
import Data.Image.Qoi.Pixel
import Data.Image.Qoi.Util

data ChunkResult pixel
  = One pixel
  | Repeat pixel Int
  | Lookback Int
  | Stop

peekChunk :: Pixel pixel => BS.ByteString -> Int -> pixel -> (Int, ChunkResult pixel)
peekChunk str pos prevPixel
  | byte .>>. 6 == 0     = (1, Lookback $ fromIntegral $ byte .&. 0b00111111)
  | byte .>>. 5 == 0b010 = (1, Repeat prevPixel $ fromIntegral $ 1 + byte .&. 0b00011111)
  | byte .>>. 5 == 0b011 = (2, Repeat prevPixel $ 33 + (fromIntegral (byte .&. 0b00011111) .<<. 8
                                                    .|. fromIntegral (str ! pos + 1)
                                                       )
                           )
  | byte .>>. 6 == 0b10  = let dr = (byte .>>. 4 .&. 0b11) - 2
                               dg = (byte .>>. 2 .&. 0b11) - 2
                               db = (byte        .&. 0b11) - 2
                            in (1, One $ addRGB prevPixel dr dg db)
  | byte .>>. 5 == 0b110 = let next = str ! pos + 1
                               dr = (byte .&. 0b00011111) - 16
                               dg = (next .>>. 4)         - 8
                               db = (next .&. 0b00001111) - 8
                            in (2, One $ addRGB prevPixel dr dg db)
  | byte .>>. 4 == 0b1110 = let threeBytes :: Word32
                                threeBytes = fromIntegral byte .<<. 16
                                         .|. fromIntegral (str ! pos + 1) .<<. 8
                                         .|. fromIntegral (str ! pos + 2)
                                dr = fromIntegral (threeBytes .>>. 15 .&. 0b11111) - 16
                                dg = fromIntegral (threeBytes .>>. 10 .&. 0b11111) - 16
                                db = fromIntegral (threeBytes .>>. 5  .&. 0b11111) - 16
                                da = fromIntegral (threeBytes         .&. 0b11111) - 16
                             in (3, One $ addRGBA prevPixel dr dg db da)
  | byte .>>. 4 == 0b1111 = let hr = byte .>>. 3 .&. 0b1
                                hg = byte .>>. 2 .&. 0b1
                                hb = byte .>>. 1 .&. 0b1
                                ha = byte        .&. 0b1
                                (r, g, b, a) = toRGBA prevPixel
                                r' = (negate hr .&. (str ! pos + 1))
                                 .|. (hr - 1)   .&. r
                                g' = (negate hg .&. (str ! pos + 1 + fromIntegral hr))
                                 .|. (hg - 1)   .&. g
                                b' = (negate hb .&. (str ! pos + 1 + fromIntegral (hr + hg)))
                                 .|. (hb - 1)   .&. b
                                a' = (negate ha .&. (str ! pos + 1 + fromIntegral (hr + hg + hb)))
                                 .|. (ha - 1)   .&. a
                             in (1 + fromIntegral (hr + hg + hb + ha), One $ fromRGBA r' g' b' a')
  | otherwise = (0, Stop)
  where
    byte = str ! pos

decodePixels :: Pixel pixel => BS.ByteString -> Int -> Int -> A.UArray Int pixel
decodePixels str strFrom n = A.runSTUArray $ do
  (mvec :: A.STUArray s Int pixel) <- A.unsafeNewArray_ (0, n - 1)

  running <- A.newArray @(A.STUArray s) (0, 63 :: Int) initPixel

  let step inPos outPos prevPixel
        | outPos < n = do
            let (diff, chunk) = peekChunk str inPos prevPixel
            case chunk of
                 One px        -> do A.unsafeWrite mvec outPos px
                                     updateRunning running px
                                     step (inPos + diff) (outPos + 1)   px
                 Lookback pos  -> do px <- A.unsafeRead running pos
                                     A.unsafeWrite mvec outPos px
                                     step (inPos + diff) (outPos + 1)   px
                 Repeat px cnt -> do forM_ [0..cnt - 1] $ \i -> A.unsafeWrite mvec (outPos + i) px
                                     updateRunning running px
                                     step (inPos + diff) (outPos + cnt) px
                 Stop          -> pure outPos
        | otherwise = pure outPos
  finish <- step strFrom 0 initPixel

  forM_ [finish .. n - 1] $ \i -> A.unsafeWrite mvec i initPixel

  pure mvec

data SomePixels where
  Pixels3 :: A.UArray Int Pixel3 -> SomePixels
  Pixels4 :: A.UArray Int Pixel4 -> SomePixels

data DecodeError
  = HeaderError String
  | UnsupportedChannels Int
  | UnpaddedFile
  deriving (Show)

decodeWHeader :: BS.ByteString
              -> Either (BSL.ByteString, ByteOffset, String) (BSL.ByteString, ByteOffset, Header)
              -> Either DecodeError (Header, SomePixels)
decodeWHeader _ (Left (_, _, err)) = Left $ HeaderError err
decodeWHeader str (Right (_, consumed, header))
  | any (\i -> (str ! BS.length str - i) /= 0) [1..4] = Left UnpaddedFile
  | hChannels header == 3 = Right (header, Pixels3 decode')
  | hChannels header == 4 = Right (header, Pixels4 decode')
  | otherwise = Left $ UnsupportedChannels $ fromIntegral $ hChannels header
  where
    decode' :: Pixel pixel => A.UArray Int pixel
    decode' = decodePixels str (fromIntegral consumed) (fromIntegral $ hWidth header * hHeight header)

decodeQoi :: BS.ByteString -> Either DecodeError (Header, SomePixels)
decodeQoi str = decodeWHeader str $ decodeOrFail $ BSL.fromStrict $ BS.take 14 str
