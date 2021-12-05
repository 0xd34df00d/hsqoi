{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Data.Image.Qoi.Format where

import Data.Binary
import Data.Binary.Combinators
import GHC.Generics

data Header = Header
  { hMagic :: MatchBytes "QOI magic" '[ 0x71, 0x6f, 0x69, 0x66 ]
  , hWidth :: Word32
  , hHeight :: Word32
  , hChannels :: Word8
  , hColorspace :: Word8
  } deriving (Eq, Show, Generic, Binary)
