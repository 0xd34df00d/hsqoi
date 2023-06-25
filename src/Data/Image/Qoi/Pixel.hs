{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -O2 #-}

module Data.Image.Qoi.Pixel where

import qualified Data.Array.Base as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Control.Monad.ST
import Data.Bits
import Data.Word
import Foreign
import GHC.Base
import GHC.ByteOrder
import GHC.ST
import GHC.Word

import Data.Image.Qoi.Util

data Pixel3 = Pixel3 Word8 Word8 Word8 deriving (Show, Eq)
newtype Pixel4 = Pixel4 Word32 deriving (Show, Eq)

instance A.MArray (A.STUArray s) Pixel3 (ST s) where
  getBounds (A.STUArray l u _ _) = pure (l, u)
  {-# INLINE getBounds #-}
  getNumElements (A.STUArray _ _ n _) = pure n
  {-# INLINE getNumElements #-}

  newArray_ arrBounds = A.newArray arrBounds (Pixel3 0 0 0)
  {-# INLINE newArray_ #-}
  unsafeNewArray_ (l, u) = A.unsafeNewArraySTUArray_ (l, u) (*# 3#)
  {-# INLINE unsafeNewArray_ #-}

  unsafeRead (A.STUArray _ _ _ marr#) (I# n#) = ST $ \s1# ->
    let n'# = n# *# 3#
        !(# s2#, r# #) = readWord8Array# marr# n'#         s1#
        !(# s3#, g# #) = readWord8Array# marr# (n'# +# 1#) s2#
        !(# s4#, b# #) = readWord8Array# marr# (n'# +# 2#) s3#
     in (# s4#, Pixel3 (W8# r#) (W8# g#) (W8# b#) #)
  {-# INLINE unsafeRead #-}
  unsafeWrite (A.STUArray _ _ _ marr#) (I# n#) (Pixel3 (W8# r#) (W8# g#) (W8# b#)) = ST $ \s1# ->
    let n'# = n# *# 3#
        s2# = writeWord8Array# marr# n'#         r# s1#
        s3# = writeWord8Array# marr# (n'# +# 1#) g# s2#
        s4# = writeWord8Array# marr# (n'# +# 2#) b# s3#
     in (# s4#, () #)
  {-# INLINE unsafeWrite #-}

instance A.IArray A.UArray Pixel3 where
  bounds (A.UArray l u _ _) = (l, u)
  {-# INLINE bounds #-}
  numElements (A.UArray  _ _ n _) = n
  {-# INLINE numElements #-}
  unsafeArray lu ies = runST (A.unsafeArrayUArray lu ies $ Pixel3 0 0 0)
  {-# INLINE unsafeArray #-}
  unsafeAt (A.UArray _ _ _ arr#) (I# n#) = Pixel3 (W8# (indexWord8Array# arr# n'#))
                                                  (W8# (indexWord8Array# arr# (n'# +# 1#)))
                                                  (W8# (indexWord8Array# arr# (n'# +# 2#)))
    where
      n'# = n# *# 3#
  {-# INLINE unsafeAt #-}

instance A.MArray (A.STUArray s) Pixel4 (ST s) where
  getBounds (A.STUArray l u _ _) = pure (l, u)
  {-# INLINE getBounds #-}
  getNumElements (A.STUArray _ _ n _) = pure n
  {-# INLINE getNumElements #-}

  newArray_ arrBounds = A.newArray arrBounds (Pixel4 0)
  {-# INLINE newArray_ #-}
  unsafeNewArray_ (l, u) = A.unsafeNewArraySTUArray_ (l, u) (*# 4#)
  {-# INLINE unsafeNewArray_ #-}

  unsafeRead (A.STUArray _ _ _ marr#) (I# n#) = ST $ \s1# ->
    let !(# s2#, rgba# #) = readWord32Array# marr# n# s1#
     in (# s2#, Pixel4 (W32# rgba#) #)
  {-# INLINE unsafeRead #-}
  unsafeWrite (A.STUArray _ _ _ marr#) (I# n#) (Pixel4 (W32# rgba#)) = ST $ \s1# ->
    let s2# = writeWord32Array# marr# n# rgba# s1#
     in (# s2#, () #)
  {-# INLINE unsafeWrite #-}

instance A.IArray A.UArray Pixel4 where
  bounds (A.UArray l u _ _) = (l, u)
  {-# INLINE bounds #-}
  numElements (A.UArray  _ _ n _) = n
  {-# INLINE numElements #-}
  unsafeArray lu ies = runST (A.unsafeArrayUArray lu ies $ Pixel4 0)
  {-# INLINE unsafeArray #-}
  unsafeAt (A.UArray _ _ _ arr#) (I# n#) = Pixel4 (W32# (indexWord32Array# arr# n#))
  {-# INLINE unsafeAt #-}

class (Eq a, forall s. A.MArray (A.STUArray s) a (ST s)) => Pixel a where
  toRGBA :: a -> (Word8, Word8, Word8, Word8)
  fromRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> a

  readPixel :: BS.ByteString -> Int -> a
  channelCount :: proxy a -> Int

bytize :: Word32 -> Word32
bytize = (.&. 0b11111111)

instance Pixel Pixel3 where
  toRGBA (Pixel3 r g b) = (r, g, b, 255)
  {-# INLINE toRGBA #-}
  fromRGBA r g b _ = Pixel3 r g b
  {-# INLINE fromRGBA #-}

  readPixel str pos = Pixel3 (str ! pos) (str ! pos + 1) (str ! pos + 2)
  {-# INLINE readPixel #-}
  channelCount _ = 3
  {-# INLINE channelCount #-}

rShift, gShift, bShift, aShift :: Int
(rShift, gShift, bShift, aShift) = case targetByteOrder of
                                        BigEndian -> (24, 16, 8, 0)
                                        LittleEndian -> (0, 8, 16, 24)

instance Pixel Pixel4 where
  toRGBA (Pixel4 rgba) = ( fromIntegral $ rgba .>>. rShift
                         , fromIntegral $ rgba .>>. gShift
                         , fromIntegral $ rgba .>>. bShift
                         , fromIntegral $ rgba .>>. aShift
                         )
  {-# INLINE toRGBA #-}
  fromRGBA r g b a = Pixel4 $ fromIntegral r .<<. rShift
                          .|. fromIntegral g .<<. gShift
                          .|. fromIntegral b .<<. bShift
                          .|. fromIntegral a .<<. aShift
  {-# INLINE fromRGBA #-}

  readPixel (BSI.PS x _ _) pos = Pixel4 $ BSI.accursedUnutterablePerformIO $ BSI.unsafeWithForeignPtr x $ \p -> peek (p `plusPtr` pos)
  {-# INLINE readPixel #-}
  channelCount _ = 4
  {-# INLINE channelCount #-}

addRGB :: Pixel pixel => pixel -> Word8 -> Word8 -> Word8 -> pixel
addRGB px dr dg db = addRGBA px dr dg db 0
{-# INLINE addRGB #-}

addRGBA :: Pixel pixel => pixel -> Word8 -> Word8 -> Word8 -> Word8 -> pixel
addRGBA px dr dg db da = let (r, g, b, a) = toRGBA px
                          in fromRGBA (r + dr) (g + dg) (b + db) (a + da)
{-# INLINE addRGBA #-}

pixelHash :: (Num a, Pixel pixel) => pixel -> a
pixelHash px = fromIntegral $ (r * 3 +  g * 5 +  b * 7 + a * 11) .&. 0b00111111
  where (r, g, b, a) = toRGBA px
{-# INLINE pixelHash #-}

updateRunning :: Pixel pixel => A.STUArray s Int pixel -> pixel -> ST s ()
updateRunning running px = A.unsafeWrite running (pixelHash px) px
{-# INLINE updateRunning #-}
