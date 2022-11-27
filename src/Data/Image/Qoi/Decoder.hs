{-# LANGUAGE Strict #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -O2 #-}

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
import Data.Proxy

import Data.Image.Qoi.Format
import Data.Image.Qoi.Pixel
import Data.Image.Qoi.Util

data ChunkResult pixel
  = One pixel
  | Repeat Int
  | Lookback Int
  | Stop

peekChunk :: forall pixel. Pixel pixel => BS.ByteString -> Int -> pixel -> (Int, ChunkResult pixel)
peekChunk str pos prevPixel
  | byte == 0b11111110 = let r' = str ! pos + 1
                             g' = str ! pos + 2
                             b' = str ! pos + 3
                             (_, _, _, a) = toRGBA prevPixel
                          in (4, One $ fromRGBA r' g' b' a)
  | channelCount (Proxy :: Proxy pixel) == 4 &&
    byte == 0b11111111 = let r' = str ! pos + 1
                             g' = str ! pos + 2
                             b' = str ! pos + 3
                             a' = str ! pos + 4
                          in (5, One $ fromRGBA r' g' b' a')
  | otherwise = case byte .>>. 6 of
                     0b00 -> (1, Lookback $ fromIntegral $ byte .&. 0b00111111)
                     0b11 -> (1, Repeat $ fromIntegral $ 1 + byte .&. 0b00111111)
                     0b01 -> let dr = byte .>>. 4 .&. 0b11 - 2
                                 dg = byte .>>. 2 .&. 0b11 - 2
                                 db = byte        .&. 0b11 - 2
                              in (1, One $ addRGB prevPixel dr dg db)
                     0b10 -> let dg = (byte .&. 0b00111111) - 32
                                 nextByte = str ! pos + 1
                                 dr = nextByte .>>. 4         - 8 + dg
                                 db = nextByte .&. 0b00001111 - 8 + dg
                              in (2, One $ addRGB prevPixel dr dg db)
                     _ -> error "can't happen"
  where
    byte = str ! pos
{-# INLINE peekChunk #-}

decodePixels :: Pixel pixel => BS.ByteString -> Int -> Int -> A.UArray Int pixel
decodePixels str strFrom n = A.runSTUArray $ do
  (mvec :: A.STUArray s Int pixel) <- A.unsafeNewArray_ (0, n - 1 + maxRunLen)

  running <- A.newArray @(A.STUArray s) (0, 63 :: Int) (fromRGBA 0 0 0 255)

  let step inPos outPos prevPixel
        | outPos < n = do
            let (diff, chunk) = peekChunk str inPos prevPixel
            case chunk of
                 One px       -> do A.unsafeWrite mvec outPos px
                                    updateRunning running px
                                    step (inPos + diff) (outPos + 1)   px
                 Lookback pos -> do px <- A.unsafeRead running pos
                                    A.unsafeWrite mvec outPos px
                                    step (inPos + diff) (outPos + 1)   px
                 Repeat cnt   -> do forM_ [0 .. cnt - 1] $ \i -> A.unsafeWrite mvec (outPos + i) prevPixel
                                    step (inPos + diff) (outPos + cnt) prevPixel
                 Stop         -> pure outPos
        | otherwise = pure outPos
  finish <- step strFrom 0 (fromRGBA 0 0 0 255)

  forM_ [finish .. n - 1] $ \i -> A.unsafeWrite mvec i (fromRGBA 0 0 0 255)

  pure $ unsafeShrink mvec n
{-# INLINE decodePixels #-}

data SomePixels
  = Pixels3 (A.UArray Int Pixel3)
  | Pixels4 (A.UArray Int Pixel4)
  deriving (Show)

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
  | any (\i -> (str ! BS.length str - i) /= 0) [2..8] ||
    BS.last str /= 1 = Left UnpaddedFile
  | hChannels header == 3 = Right (header, Pixels3 decode')
  | hChannels header == 4 = Right (header, Pixels4 decode')
  | otherwise = Left $ UnsupportedChannels $ fromIntegral $ hChannels header
  where
    decode' :: Pixel pixel => A.UArray Int pixel
    decode' = decodePixels str (fromIntegral consumed) (fromIntegral $ hWidth header * hHeight header)
    {-# INLINE decode' #-}

decodeQoi :: BS.ByteString -> Either DecodeError (Header, SomePixels)
decodeQoi str = decodeWHeader (unoffsetBS str) $ decodeOrFail $ BSL.fromStrict $ BS.take 14 str
{-# NOINLINE decodeQoi #-}
