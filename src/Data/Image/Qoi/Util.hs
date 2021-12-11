{-# LANGUAGE Strict #-}
{-# LANGUAGE BinaryLiterals #-}

module Data.Image.Qoi.Util where

import qualified Data.Array.Base as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Bits
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable (peekByteOff)

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

unsafeShrink :: A.STUArray s Int Word8 -> Int -> A.STUArray s Int Word8
unsafeShrink arr@(A.STUArray l _ n marr) cnt
  | cnt >= n = arr
  | otherwise = A.STUArray l (l + cnt - 1) cnt marr
