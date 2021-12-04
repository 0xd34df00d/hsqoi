{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Data.Image.Qoi.Pixel where

import Data.Vector.Unboxed.Deriving
import Data.Word

data Pixel3 = Pixel3 Word8 Word8 Word8 deriving (Show)
data Pixel4 = Pixel4 Word8 Word8 Word8 Word8 deriving (Show)

addPixel3 :: Pixel3 -> Pixel3 -> Pixel3
addPixel3 (Pixel3 r1 g1 b1) (Pixel3 r2 g2 b2) = Pixel3 (r1 + r2) (g1 + g2) (b1 + b2)

derivingUnbox "Pixel3"
  [t| Pixel3 -> (Word8, Word8, Word8) |]
  [| \(Pixel3 r g b) -> (r, g, b) |]
  [| \(r, g, b) -> Pixel3 r g b |]
