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
{-# OPTIONS_GHC -O2 -fllvm #-}

module Data.Image.Qoi.Pixel where

import qualified Data.Array.Base as A
import qualified Data.ByteString as BS
import Control.Monad.ST
import Data.Bits
import Data.Word
import GHC.Base
import GHC.ST
import GHC.Word

import Data.Image.Qoi.Util

data Pixel3 = Pixel3 Word8 Word8 Word8 deriving (Show, Eq)
data Pixel4 = Pixel4 Word8 Word8 Word8 Word8 deriving (Show, Eq)

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

  newArray_ arrBounds = A.newArray arrBounds (Pixel4 0 0 0 0)
  {-# INLINE newArray_ #-}
  unsafeNewArray_ (l, u) = A.unsafeNewArraySTUArray_ (l, u) (*# 4#)
  {-# INLINE unsafeNewArray_ #-}

  unsafeRead (A.STUArray _ _ _ marr#) (I# n#) = ST $ \s1# ->
    let n'# = n# *# 4#
        !(# s2#, r# #) = readWord8Array# marr# n'#         s1#
        !(# s3#, g# #) = readWord8Array# marr# (n'# +# 1#) s2#
        !(# s4#, b# #) = readWord8Array# marr# (n'# +# 2#) s3#
        !(# s5#, a# #) = readWord8Array# marr# (n'# +# 3#) s4#
     in (# s5#, Pixel4 (W8# r#) (W8# g#) (W8# b#) (W8# a#) #)
  {-# INLINE unsafeRead #-}
  unsafeWrite (A.STUArray _ _ _ marr#) (I# n#) (Pixel4 (W8# r#) (W8# g#) (W8# b#) (W8# a#)) = ST $ \s1# ->
    let n'# = n# *# 4#
        s2# = writeWord8Array# marr# n'#         r# s1#
        s3# = writeWord8Array# marr# (n'# +# 1#) g# s2#
        s4# = writeWord8Array# marr# (n'# +# 2#) b# s3#
        s5# = writeWord8Array# marr# (n'# +# 3#) a# s4#
     in (# s5#, () #)
  {-# INLINE unsafeWrite #-}

instance A.IArray A.UArray Pixel4 where
  bounds (A.UArray l u _ _) = (l, u)
  {-# INLINE bounds #-}
  numElements (A.UArray  _ _ n _) = n
  {-# INLINE numElements #-}
  unsafeArray lu ies = runST (A.unsafeArrayUArray lu ies $ Pixel4 0 0 0 0)
  {-# INLINE unsafeArray #-}
  unsafeAt (A.UArray _ _ _ arr#) (I# n#) = Pixel4 (W8# (indexWord8Array# arr# n'#))
                                                  (W8# (indexWord8Array# arr# (n'# +# 1#)))
                                                  (W8# (indexWord8Array# arr# (n'# +# 2#)))
                                                  (W8# (indexWord8Array# arr# (n'# +# 3#)))
    where
      n'# = n# *# 4#
  {-# INLINE unsafeAt #-}

class (Eq a, forall s. A.MArray (A.STUArray s) a (ST s)) => Pixel a where
  addRGB  :: a -> Word8 -> Word8 -> Word8 -> a
  addRGBA :: a -> Word8 -> Word8 -> Word8 -> Word8 -> a

  toRGBA :: a -> (Word8, Word8, Word8, Word8)
  fromRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> a

  readPixel :: BS.ByteString -> Int -> a

instance Pixel Pixel3 where
  addRGB (Pixel3 r g b) dr dg db = Pixel3 (r + dr) (g + dg) (b + db)
  {-# INLINE addRGB #-}
  addRGBA (Pixel3 r g b) dr dg db _ = Pixel3 (r + dr) (g + dg) (b + db)
  {-# INLINE addRGBA #-}
  toRGBA (Pixel3 r g b) = (r, g, b, 255)
  {-# INLINE toRGBA #-}
  fromRGBA r g b _ = Pixel3 r g b
  {-# INLINE fromRGBA #-}

  readPixel str pos = Pixel3 (str ! pos) (str ! pos + 1) (str ! pos + 2)
  {-# INLINE readPixel #-}

instance Pixel Pixel4 where
  addRGB (Pixel4 r g b a) dr dg db = Pixel4 (r + dr) (g + dg) (b + db) a
  {-# INLINE addRGB #-}
  addRGBA (Pixel4 r g b a) dr dg db da = Pixel4 (r + dr) (g + dg) (b + db) (a + da)
  {-# INLINE addRGBA #-}
  toRGBA (Pixel4 r g b a) = (r, g, b, a)
  {-# INLINE toRGBA #-}
  fromRGBA r g b a = Pixel4 r g b a
  {-# INLINE fromRGBA #-}

  readPixel str pos = Pixel4 (str ! pos) (str ! pos + 1) (str ! pos + 2) (str ! pos + 3)
  {-# INLINE readPixel #-}

pixelHash :: (Num a, Pixel pixel) => pixel -> a
pixelHash px = fromIntegral $ (r `xor` g `xor` b `xor` a) .&. 0b00111111
  where (r, g, b, a) = toRGBA px
{-# INLINE pixelHash #-}

updateRunning :: Pixel pixel => A.STUArray s Int pixel -> pixel -> ST s ()
updateRunning running px = A.unsafeWrite running (pixelHash px) px
{-# INLINE updateRunning #-}
