{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 -fllvm #-}

module Data.Image.Qoi.Pixel where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Generic.Base as VG
import qualified Data.Vector.Generic.Mutable.Base as VMG
import Data.Vector.Unboxed.Deriving
import Data.Bits
import Data.Word

data Pixel3 = Pixel3 Word8 Word8 Word8 deriving (Show)
data Pixel4 = Pixel4 Word8 Word8 Word8 Word8 deriving (Show)

newtype instance VM.MVector s Pixel3 = MV_Pixel3 { getMVP3 :: VM.MVector s Word8 }
newtype instance V.Vector     Pixel3 = V_Pixel3  { getVP3  :: V.Vector Word8 }

instance VG.Vector V.Vector Pixel3 where
  basicUnsafeFreeze = fmap V_Pixel3 . VG.basicUnsafeFreeze . getMVP3
  basicUnsafeThaw = fmap MV_Pixel3 . VG.basicUnsafeThaw . getVP3
  basicLength = (`div` 3) . VG.basicLength . getVP3
  basicUnsafeSlice s l = V_Pixel3 . VG.basicUnsafeSlice (s * 3) (l * 3) . getVP3
  basicUnsafeIndexM (V_Pixel3 vec) idx = Pixel3 <$> VG.basicUnsafeIndexM vec idx'
                                                <*> VG.basicUnsafeIndexM vec (idx' + 1)
                                                <*> VG.basicUnsafeIndexM vec (idx' + 2)
    where
      idx' = idx * 3
  elemseq _ !px b = b

instance VMG.MVector VM.MVector Pixel3 where
  basicLength = (`div` 3) . VMG.basicLength . getMVP3
  basicUnsafeSlice s l = MV_Pixel3 . VMG.basicUnsafeSlice (s * 3) (l * 3) . getMVP3
  basicOverlaps (MV_Pixel3 v1) (MV_Pixel3 v2) = VMG.basicOverlaps v1 v2
  basicUnsafeNew = fmap MV_Pixel3 . VMG.basicUnsafeNew . (* 3)
  basicInitialize = VMG.basicInitialize . getMVP3
  basicUnsafeRead (MV_Pixel3 vec) idx = Pixel3 <$> VMG.basicUnsafeRead vec idx'
                                               <*> VMG.basicUnsafeRead vec (idx' + 1)
                                               <*> VMG.basicUnsafeRead vec (idx' + 2)
    where
      idx' = idx * 3
  basicUnsafeWrite (MV_Pixel3 vec) idx (Pixel3 r g b) = VMG.basicUnsafeWrite vec idx' r
                                                     >> VMG.basicUnsafeWrite vec (idx' + 1) g
                                                     >> VMG.basicUnsafeWrite vec (idx' + 2) b
    where
      idx' = idx * 3

instance VM.Unbox Pixel3

derivingUnbox "Pixel4"
  [t| Pixel4 -> Word32 |]
  [| \(Pixel4 r g b a) -> (fromIntegral r `shiftL` 24)
                      .|. (fromIntegral g `shiftL` 16)
                      .|. (fromIntegral b `shiftL` 8)
                      .|.  fromIntegral a
                      |]
  [| \w32 -> Pixel4 (fromIntegral $ w32 `shiftR` 24)
                    (fromIntegral $ w32 `shiftR` 16)
                    (fromIntegral $ w32 `shiftR` 8)
                    (fromIntegral   w32)
                    |]

class VM.Unbox a => Pixel a where
  initPixel :: a

  addRGB  :: a -> Word8 -> Word8 -> Word8 -> a
  addRGBA :: a -> Word8 -> Word8 -> Word8 -> Word8 -> a

  toRGBA :: a -> (Word8, Word8, Word8, Word8)
  fromRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> a

instance Pixel Pixel3 where
  initPixel = Pixel3 0 0 0

  addRGB (Pixel3 r g b) dr dg db = Pixel3 (r + dr) (g + dg) (b + db)
  addRGBA (Pixel3 r g b) dr dg db _ = Pixel3 (r + dr) (g + dg) (b + db)
  toRGBA (Pixel3 r g b) = (r, g, b, 255)
  fromRGBA r g b _ = Pixel3 r g b

instance Pixel Pixel4 where
  initPixel = Pixel4 0 0 0 255

  addRGB (Pixel4 r g b a) dr dg db = Pixel4 (r + dr) (g + dg) (b + db) a
  addRGBA (Pixel4 r g b a) dr dg db da = Pixel4 (r + dr) (g + dg) (b + db) (a + da)
  toRGBA (Pixel4 r g b a) = (r, g, b, a)
  fromRGBA r g b a = Pixel4 r g b a
