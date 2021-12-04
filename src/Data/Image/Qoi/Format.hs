{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Image.Qoi.Format where

import Data.Store
import Data.Word
import GHC.Generics

data Header = Header
  { hMagic :: Word32
  , hWidth :: Word32
  , hHeight :: Word32
  , hChannels :: Word8
  , hColorspace :: Word8
  } deriving (Eq, Show, Generic, Store)
