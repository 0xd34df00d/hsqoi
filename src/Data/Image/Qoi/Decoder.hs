{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -O2 -fllvm #-}

module Data.Image.Qoi.Decoder
( decodeQoi
, SomePixels(..)
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Arrow
import Data.Bits
import Data.Store
import Data.Word
import System.ByteOrder

import Data.Image.Qoi.Format
import Data.Image.Qoi.Pixel

infix 4 !
(!) :: BS.ByteString -> Int -> Word8
(!) = BS.unsafeIndex

infix 8 .>>., .<<.
(.>>.), (.<<.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
(.<<.) = shiftL

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
                                 .|. (hr - 1)    .&. r
                                g' = (negate hg .&. (str ! pos + 1 + fromIntegral hr))
                                 .|. (hg - 1)    .&. g
                                b' = (negate hb .&. (str ! pos + 1 + fromIntegral (hr + hg)))
                                 .|. (hb - 1)    .&. b
                                a' = (negate ha .&. (str ! pos + 1 + fromIntegral (hr + hg + hb)))
                                 .|. (ha - 1)    .&. a
                             in (1 + fromIntegral (hr + hg + hb + ha), One $ fromRGBA r' g' b' a')
  | otherwise = (0, Stop)
  where
    byte = str ! pos

decodePixels :: Pixel pixel => BS.ByteString -> Int -> V.Vector pixel
decodePixels str n = V.create $ do
  mvec <- VM.new n

  running <- VM.replicate 64 $ initPixel

  let updateRunning px = let (r, g, b, a) = toRGBA px
                          in VM.unsafeWrite running (fromIntegral $ (r `xor` g `xor` b `xor` a) .&. 0b00111111) px

  let step inPos outPos prevPixel
        | outPos < n = do
            let (diff, chunk) = peekChunk str inPos prevPixel
            case chunk of
                 One px        -> do VM.unsafeWrite mvec outPos px
                                     updateRunning px
                                     step (inPos + diff) (outPos + 1)   px
                 Lookback pos  -> do px <- VM.unsafeRead running pos
                                     VM.unsafeWrite mvec outPos px
                                     step (inPos + diff) (outPos + 1)   px
                 Repeat px cnt -> do VM.set (VM.unsafeSlice outPos cnt mvec) px
                                     updateRunning px
                                     step (inPos + diff) (outPos + cnt) px
                 Stop          -> pure ()
        | otherwise = pure ()
  step 0 0 initPixel

  pure mvec

data SomePixels where
  Pixels3 :: V.Vector Pixel3 -> SomePixels
  Pixels4 :: V.Vector Pixel4 -> SomePixels

decodeQoi :: BS.ByteString -> Maybe (Header, SomePixels)
decodeQoi str
  | consumed /= 14 = Nothing
  | hChannels header == 3 = Just (header, Pixels3 decode')
  | hChannels header == 4 = Just (header, Pixels4 decode')
  | otherwise = Nothing
  where
    (consumed, header) = second fixupBE $ decodeExPortionWith peek str
    fixupBE Header { .. } = Header { hMagic = fromBigEndian hMagic
                                   , hWidth = fromBigEndian hWidth
                                   , hHeight = fromBigEndian hHeight
                                   , ..
                                   }
    decode' :: Pixel pixel => V.Vector pixel
    decode' = decodePixels (BS.drop consumed str) (fromIntegral $ hWidth header * hHeight header)
