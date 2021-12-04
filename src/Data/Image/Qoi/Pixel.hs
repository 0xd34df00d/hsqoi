{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Data.Image.Qoi.Pixel where

import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Unboxed.Deriving
import Data.Word

data Pixel3 = Pixel3 Word8 Word8 Word8 deriving (Show)
data Pixel4 = Pixel4 Word8 Word8 Word8 Word8 deriving (Show)

derivingUnbox "Pixel3"
  [t| Pixel3 -> (Word8, Word8, Word8) |]
  [| \(Pixel3 r g b) -> (r, g, b) |]
  [| \(r, g, b) -> Pixel3 r g b |]

derivingUnbox "Pixel4"
  [t| Pixel4 -> (Word8, Word8, Word8, Word8) |]
  [| \(Pixel4 r g b a) -> (r, g, b, a) |]
  [| \(r, g, b, a) -> Pixel4 r g b a |]

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
