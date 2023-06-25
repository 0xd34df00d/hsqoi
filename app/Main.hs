{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Array.Base as A
import qualified Data.Array.IO as A
import qualified Data.ByteString as BS
import Codec.Picture.Png
import Codec.Picture.Types
import Data.Binary.Combinators
import Data.Functor
import GHC.Word
import System.Environment
import System.IO
import System.IO.MMap

import Data.Image.Qoi.Encoder
import Data.Image.Qoi.Format
import Data.Image.Qoi.Decoder
import Data.Image.Qoi.Pixel

toImage :: Header -> SomePixels -> DynamicImage
toImage Header { .. } (Pixels3 pixels) = ImageRGB8 $ generateImage f (fromIntegral hWidth) (fromIntegral hHeight)
  where
    f x y = let (r, g, b, _) = toRGBA $ pixels A.! (y * fromIntegral hWidth + x)
             in PixelRGB8 r g b
toImage Header { .. } (Pixels4 pixels) = ImageRGBA8 $ generateImage f (fromIntegral hWidth) (fromIntegral hHeight)
  where
    f x y = let (r, g, b, a) = toRGBA $ pixels A.! (y * fromIntegral hWidth + x)
             in PixelRGBA8 r g b a

dumpUArray :: FilePath -> A.UArray Int Word8 -> IO ()
dumpUArray file arr = withFile file WriteMode $ \h -> do
  ioarr <- A.unsafeThaw arr
  A.hPutArray h ioarr (A.numElements arr)

main :: IO ()
main = getArgs >>=
  \case ["decode", inFile] -> do
           bs <- mmapFileByteString inFile Nothing
           case decodeQoi bs of
                Right (header, pixels) -> do print header
                                             case pixels of
                                                  Pixels3 pixels' -> print $ A.bounds pixels'
                                                  Pixels4 pixels' -> print $ A.bounds pixels'
                Left err -> putStrLn $ "Unable to decode: " <> show err
        ["decode", inFile, outFile] -> do
           bs <- mmapFileByteString inFile Nothing
           case decodeQoi bs of
                Left err -> putStrLn $ "Unable to decode: " <> show err
                Right (header, pixels) -> void $ writeDynamicPng outFile $ toImage header pixels
        ["encode_raw", inFile, width, height] -> do
           bs <- mmapFileByteString inFile Nothing
           let w' = read width
               h' = read height
               chans = fromIntegral $ fromIntegral (BS.length bs) `div` w' `div` h'
           print $ A.bounds $ encodeRaw (Header matchASCII w' h' chans 0) bs 0
        ["encode_raw", inFile, width, height, outFile] -> do
           bs <- mmapFileByteString inFile Nothing
           let w' = read width
               h' = read height
               chans = fromIntegral $ fromIntegral (BS.length bs) `div` w' `div` h'
           dumpUArray outFile $ encodeRaw (Header matchASCII w' h' chans 0) bs 0
        _ -> putStrLn "Wrong usage"
