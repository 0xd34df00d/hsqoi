{-# LANGUAGE Strict #-}
{-# LANGUAGE BinaryLiterals #-}

module Data.Image.Qoi.Util where

import qualified Data.Array.Base as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Control.Monad.ST
import Data.Bits
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable (peekByteOff)

import Data.Image.Qoi.Pixel

pixelHash :: (Num a, Pixel pixel) => pixel -> a
pixelHash px = fromIntegral $ (r `xor` g `xor` b `xor` a) .&. 0b00111111
  where (r, g, b, a) = toRGBA px
{-# INLINE pixelHash #-}

updateRunning :: Pixel pixel => A.STUArray s Int pixel -> pixel -> ST s ()
updateRunning running px = A.unsafeWrite running (pixelHash px) px
{-# INLINE updateRunning #-}

infix 8 .>>., .<<.
(.>>.), (.<<.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
(.<<.) = shiftL
{-# INLINE (.<<.) #-}
{-# INLINE (.>>.) #-}

infix 4 !
(!) :: BS.ByteString -> Int -> Word8
(BSI.PS x _ _) ! i = BSI.accursedUnutterablePerformIO $ withForeignPtr x $ \p -> peekByteOff p i
{-# INLINE (!) #-}
