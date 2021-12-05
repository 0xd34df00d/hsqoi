{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.Array.IArray as A
import Codec.Picture.Png
import Codec.Picture.Types
import Data.Functor
import System.Environment
import System.IO.Posix.MMap

import Data.Image.Qoi.Format
import Data.Image.Qoi.Decoder
import Data.Image.Qoi.Pixel

toImage :: Header -> SomePixels -> DynamicImage
toImage Header { .. } (Pixels3 pixels) = ImageRGB8 $ generateImage f (fromIntegral hWidth) (fromIntegral hHeight)
  where
    f x y = let Pixel3 r g b = pixels A.! (y * fromIntegral hWidth + x)
             in PixelRGB8 r g b
toImage Header { .. } (Pixels4 pixels) = ImageRGBA8 $ generateImage f (fromIntegral hWidth) (fromIntegral hHeight)
  where
    f x y = let Pixel4 r g b a = pixels A.! (y * fromIntegral hWidth + x)
             in PixelRGBA8 r g b a

main :: IO ()
main = getArgs >>=
  \case ["decode", inFile] -> do
           bs <- unsafeMMapFile inFile
           case decodeQoi bs of
                Right (header, pixels) -> do print header
                                             case pixels of
                                                  Pixels3 pixels' -> print $ A.bounds pixels'
                                                  Pixels4 pixels' -> print $ A.bounds pixels'
                Left err -> putStrLn $ "Unable to decode: " <> show err
        ["decode", inFile, outFile] -> do
           bs <- unsafeMMapFile inFile
           case decodeQoi bs of
                Left err -> putStrLn $ "Unable to decode: " <> show err
                Right (header, pixels) -> void $ writeDynamicPng outFile $ toImage header pixels
        _ -> putStrLn "Wrong usage"
