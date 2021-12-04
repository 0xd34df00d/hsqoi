{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -O2 -fllvm #-}

module Data.Image.Qoi.Decoder(decodeQoi) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Store
import Data.Word
import System.ByteOrder

import Data.Image.Qoi.Format
import Data.Image.Qoi.Pixel
import Control.Monad.ST

infix 4 !
(!) :: BS.ByteString -> Int -> Word8
(!) = BS.unsafeIndex

infix 8 .>>., .<<.
(.>>.), (.<<.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
(.<<.) = shiftL

data Chunk
  = QoiIndex Word8
  | QoiRun8 Word8
  | QoiRun16 Word16
  | QoiDiff8 Word8 Word8 Word8
  | QoiDiff16 Word8 Word8 Word8
  | QoiDiff24 Word8 Word8 Word8 Word8
  | QoiColor Word8 Word8 Word8 Word8
  deriving (Eq, Show)

peekChunk :: BS.ByteString -> Int -> (Int, Chunk)
peekChunk str pos
  | byte .>>. 6 == 0     = (1, QoiIndex $ byte .&. 0b00111111)
  | byte .>>. 5 == 0b010 = (1, QoiRun8  $ 1 + byte .&. 0b00011111)
  | byte .>>. 5 == 0b011 = (2, QoiRun16 $ 33 + (fromIntegral ((byte .&. 0b00011111) .<<. 8)
                                            .|. fromIntegral (str ! pos + 1)
                                               )
                           )
  | byte .>>. 6 == 0b10  = (1, QoiDiff8 (byte .>>. 4 .&. 0b11)
                                        (byte .>>. 2 .&. 0b11)
                                        (byte        .&. 0b11))
  | byte .>>. 5 == 0b110 = let next = str ! pos + 1 in
                           (2, QoiDiff16 (byte .&. 0b00011111)
                                         (next .>>. 4)
                                         (next .&. 0b00001111)
                           )
  | byte .>>. 4 == 0b1110 = let threeBytes :: Word32
                                threeBytes = fromIntegral byte .<<. 16
                                         .|. fromIntegral (str ! pos + 1) .<<. 8
                                         .|. fromIntegral (str ! pos + 2)
                             in (3, QoiDiff24 (fromIntegral $ threeBytes .>>. 15 .&. 0b11111)
                                              (fromIntegral $ threeBytes .>>. 10 .&. 0b11111)
                                              (fromIntegral $ threeBytes .>>. 5  .&. 0b11111)
                                              (fromIntegral $ threeBytes         .&. 0b11111)
                                )
  | byte .>>. 4 == 0b1111 = let hr = byte .>>. 3 .&. 0b1
                                hg = byte .>>. 2 .&. 0b1
                                hb = byte .>>. 1 .&. 0b1
                                ha = byte        .&. 0b1
                                r = negate hr .&. (str ! pos + 1)
                                g = negate hg .&. (str ! pos + 1 + fromIntegral hr)
                                b = negate hb .&. (str ! pos + 1 + fromIntegral (hr + hg))
                                a = negate ha .&. (str ! pos + 1 + fromIntegral (hr + hg + hb))
                             in (1 + fromIntegral (hr + hg + hb + ha), QoiColor r g b a)
  | otherwise = error "unknown byte"
  where
    byte = str ! pos

data ChunkResult pixel
  = One pixel 
  | Repeat Int pixel
  | Lookback Int

processChunk3 :: Pixel3 -> Chunk -> ChunkResult Pixel3
processChunk3 prevPixel =
  \case QoiIndex idx -> Lookback $ fromIntegral idx
        QoiRun8 len  -> Repeat (fromIntegral len) prevPixel
        QoiRun16 len -> Repeat (fromIntegral len) prevPixel
        QoiDiff8  dr dg db   -> One $ addPixel3 prevPixel (Pixel3 (dr - 2)  (dg - 2)  (db - 2))
        QoiDiff16 dr dg db   -> One $ addPixel3 prevPixel (Pixel3 (dr - 16) (dg - 8)  (db - 8))
        QoiDiff24 dr dg db _ -> One $ addPixel3 prevPixel (Pixel3 (dr - 16) (dg - 16) (db - 16))
        QoiColor  r  g  b  _ -> One $ Pixel3 r g b

updateRunning3 :: Pixel3 -> VM.MVector s Pixel3 -> ST s ()
updateRunning3 px@(Pixel3 r g b) mvec = VM.unsafeWrite mvec pos px
  where
    pos = fromIntegral $ (r `xor` g `xor` b `xor` 255) .&. 0b00111111

decode3ch :: BS.ByteString -> Int -> V.Vector Pixel3
decode3ch str n = V.create $ do
  mvec <- VM.unsafeNew n

  running <- VM.replicate 64 $ Pixel3 0 0 0
  let step inPos outPos prevPixel
        | outPos < n = do
            let (diff, chunk) = peekChunk str inPos
            case processChunk3 prevPixel chunk of
                 One px        -> do VM.unsafeWrite mvec outPos px
                                     updateRunning3 px running
                                     step (inPos + diff) (outPos + 1)   px
                 Lookback pos  -> do px <- VM.unsafeRead running pos
                                     VM.unsafeWrite mvec outPos px
                                     step (inPos + diff) (outPos + 1)   px
                 Repeat cnt px -> do VM.set (VM.unsafeSlice outPos cnt mvec) px
                                     updateRunning3 px running
                                     step (inPos + diff) (outPos + cnt) px
        | otherwise = pure ()
  step 0 0 (Pixel3 0 0 0)

  pure mvec

decodeQoi :: BS.ByteString -> Maybe (Header, V.Vector Pixel3)
decodeQoi str
  | consumed /= 14 = Nothing
  | otherwise = Just (header, decode3ch (BS.drop consumed str) (fromIntegral $ hWidth header * hHeight header))
  where
    (consumed, header) = second fixupBE $ decodeExPortionWith peek str
    fixupBE Header { .. } = Header { hMagic = fromBigEndian hMagic
                                   , hWidth = fromBigEndian hWidth
                                   , hHeight = fromBigEndian hHeight
                                   , ..
                                   }
