{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as A
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Control.Monad
import Data.Binary.Combinators
import Data.Either
import Data.Proxy
import Data.Word
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import Data.Image.Qoi.Decoder
import Data.Image.Qoi.Encoder
import Data.Image.Qoi.Format
import Data.Image.Qoi.Pixel

newtype ShowAsBytes pixel = ShowAsBytes { bytes :: BS.ByteString } deriving (Eq)

instance (Show pixel, A.IArray A.UArray pixel, Pixel pixel) => Show (ShowAsBytes pixel) where
  show = show . toArray @pixel . bytes

data Image pixel = Image
  { iWidth :: Int
  , iHeight :: Int
  , iBytes :: ShowAsBytes pixel
  } deriving (Eq)

deriving instance Show (ShowAsBytes pixel) => Show (Image pixel)

genPixel :: forall pixel. Pixel pixel => Gen pixel
genPixel = fromRGBA <$> chooseAny <*> chooseAny <*> chooseAny <*> if channelCount @pixel Proxy == 3 then pure 0 else chooseAny

genDiffBounded :: forall pixel. Pixel pixel => Bool -> Word8 -> pixel -> Gen pixel
genDiffBounded withAlpha delta px = do
  diffs <- replicateM (if withAlpha then channelCount @pixel Proxy else 3) $ choose (negate delta, delta - 1)
  case diffs of
       [dr, dg, db]     -> pure $ addRGB  px dr dg db
       [dr, dg, db, da] -> pure $ addRGBA px dr dg db da
       _ -> error "muh dependent types"

instance Pixel pixel => Arbitrary (Image pixel) where
  arbitrary = do
    maxDim <- getSize
    width <- chooseInt (1, maxDim)
    height <- chooseInt (1, maxDim)

    px0 <- genPixel

    pixels <- V.iterateNM (width * height) step (px0, 0)
    pure $ Image width height $ ShowAsBytes $ BS.pack $ V.toList $ V.concatMap (toComponents . fst) pixels
    where
      toComponents :: pixel -> V.Vector Word8
      toComponents px = if channelCount @pixel Proxy == 3
                           then let (r, g, b, _) = toRGBA px in [r, g, b]
                           else let (r, g, b, a) = toRGBA px in [r, g, b, a]

      step (prevPixel, 0) = do
        runToss <- chooseInt (1, 100)
        if runToss >= 10
           then do nextPixel <- oneof [ genDiffBounded False 2 prevPixel
                                      , genDiffBounded False 8 prevPixel
                                      , genDiffBounded True 16 prevPixel
                                      , genPixel
                                      ]
                   pure (nextPixel, 0)
           else (prevPixel,) <$> chooseInt (1, runToss * 1000)
      step (prevPixel, n) = pure (prevPixel, n - 1)

  shrink Image { .. } = [ Image
                          { iWidth = iWidth
                          , iHeight = iHeight - 1
                          , iBytes = ShowAsBytes $ dropIthRow i $ bytes iBytes
                          }
                        | i <- [ 0 .. iHeight - 1 ]
                        ]
                        <>
                        [ Image
                          { iWidth = iWidth - 1
                          , iHeight = iHeight
                          , iBytes = ShowAsBytes $ dropJthCol j $ bytes iBytes
                          }
                        | iHeight < 5
                        , j <- [0 .. iWidth - 1]
                        ]
    where
      chans = channelCount @pixel Proxy
      dropIthRow i str = BS.take (iWidth * chans * i) str
                      <> BS.drop (iWidth * chans * (i + 1)) str
      dropJthCol j str
        | BS.null str = str
        | otherwise = let (row, rest) = BS.splitAt (iWidth * chans) str
                          (left, right) = BS.splitAt (j * chans) row
                       in left <> BS.drop chans right <> dropJthCol j rest

toArray :: forall pixel. (Pixel pixel, A.IArray A.UArray pixel) => BS.ByteString -> A.UArray Int pixel
toArray bs = A.array (0, pxCnt - 1) [ (i, fromRGBA r g b a)
                                    | i <- [0..pxCnt - 1]
                                    , let r = bs `BS.index` (i * chans)
                                          g = bs `BS.index` (i * chans + 1)
                                          b = bs `BS.index` (i * chans + 2)
                                          a = if chans == 4
                                                 then bs `BS.index` (i * chans + 3)
                                                 else 0
                                    ]
  where
    chans = channelCount @pixel Proxy
    pxCnt = BS.length bs `div` chans

imgProperty :: forall pixel. (Show pixel, Pixel pixel, A.IArray A.UArray pixel) => Image pixel -> IO ()
imgProperty Image { .. } = do
  let header = Header { hMagic = matchASCII
                      , hWidth = fromIntegral iWidth
                      , hHeight = fromIntegral iHeight
                      , hChannels = chans
                      , hColorspace = 0
                      }
  let encoded = encodeRaw header (bytes iBytes) 0
      decoded = decodeQoi $ BS.pack $ A.elems encoded
  decoded `shouldSatisfy` isRight
  case decoded of
       Left _ -> pure ()
       Right (header', Pixels3 pixels') -> do
         chans `shouldBe` 3
         header' `shouldBe` header
         pixels' `shouldBe` toArray (bytes iBytes)
       Right (header', Pixels4 pixels') -> do
         chans `shouldBe` 4
         header' `shouldBe` header
         pixels' `shouldBe` toArray (bytes iBytes)
  where
    chans = fromIntegral $ channelCount @pixel Proxy

main :: IO ()
main = hspec $ modifyMaxSuccess (const 100000) $ do
  describe "QOI encoder (3-channel)" $
    parallel $ forM_ ([1 .. workers] :: [Int]) $ \wrk ->
      it ("decode . encode = id (worker " <> show wrk <> ")") $ property (imgProperty @Pixel3)
  describe "QOI encoder (4-channel)" $
    parallel $ forM_ ([1 .. workers] :: [Int]) $ \wrk ->
      it ("decode . encode = id (worker " <> show wrk <> ")") $ property (imgProperty @Pixel4)
  where
    workers = 10
