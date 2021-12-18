{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Data.Image.Qoi.Format where

import Data.Binary
import Data.Binary.Combinators
import GHC.Generics

data Header = Header
  { hMagic :: MatchASCII "QOI magic" "qoif"
  , hWidth :: Word32
  , hHeight :: Word32
  , hChannels :: Word8
  , hColorspace :: Word8
  } deriving (Eq, Show, Generic, Binary)
