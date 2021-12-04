{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as V
import Codec.Picture.Png
import Codec.Picture.Types
import System.Environment
import System.IO.Posix.MMap

import Data.Image.Qoi.Format
import Data.Image.Qoi.Decoder
import Data.Image.Qoi.Pixel

toImage3 :: Header -> V.Vector Pixel3 -> Image PixelRGB8
toImage3 Header { .. } pixels = generateImage f (fromIntegral hWidth) (fromIntegral hHeight)
  where
    f x y = let Pixel3 r g b = pixels V.! (y * fromIntegral hWidth + x)
             in PixelRGB8 r g b

main :: IO ()
main = getArgs >>=
  \case ["decode", inFile] -> do
           bs <- unsafeMMapFile inFile
           case decodeQoi bs of
                Just (header, pixels) -> do print header
                                            print $ V.length pixels
                Nothing -> putStrLn "Unable to decode"
        ["decode", inFile, outFile] -> do
           bs <- unsafeMMapFile inFile
           case decodeQoi bs of
                Nothing -> putStrLn "Unable to decode"
                Just (header, pixels) -> writePng outFile $ toImage3 header pixels
        _ -> putStrLn "Wrong usage"
