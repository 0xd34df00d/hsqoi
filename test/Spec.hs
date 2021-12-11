{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}

import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as A
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Control.Monad
import Data.Binary.Combinators
import Data.Either
import Data.Word
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import Data.Image.Qoi.Decoder
import Data.Image.Qoi.Encoder
import Data.Image.Qoi.Format
import Data.Image.Qoi.Pixel

newtype ShowAsBytes = ShowAsBytes { bytes :: BS.ByteString } deriving (Eq)

instance Show ShowAsBytes where
  show = show . toArray3 . bytes

data Image3 = Image3
  { iWidth :: Int
  , iHeight :: Int
  , iBytes :: ShowAsBytes
  } deriving (Eq, Show)

genPixel3 :: Gen Pixel3
genPixel3 = Pixel3 <$> chooseAny <*> chooseAny <*> chooseAny

genDiff3Bounded :: Word8 -> Pixel3 -> Gen Pixel3
genDiff3Bounded delta (Pixel3 r g b) = do
  diffs <- replicateM 3 $ choose (negate delta, delta - 1)
  case diffs of
       [dr, dg, db] -> pure $ Pixel3 (r + dr) (g + dg) (b + db)
       _ -> error "muh dependent types"

instance Arbitrary Image3 where
  arbitrary = do
    maxDim <- getSize
    width <- chooseInt (1, maxDim)
    height <- chooseInt (1, maxDim)

    px0 <- genPixel3

    pixels <- V.iterateNM (width * height) step px0
    pure $ Image3 width height $ ShowAsBytes $ BS.pack $ V.toList $ V.concatMap (\(Pixel3 r g b) -> [r, g, b]) pixels
    where
      step prevPixel = oneof [ pure prevPixel
                             , genDiff3Bounded 2 prevPixel
                             , genDiff3Bounded 8 prevPixel
                             , genDiff3Bounded 16 prevPixel
                             , genPixel3
                             ]

toArray3 :: BS.ByteString -> A.UArray Int Pixel3
toArray3 bs = A.array (0, pxCnt - 1) [ (i, Pixel3 r g b)
                                     | i <- [0..pxCnt - 1]
                                     , let r = bs `BS.index` (i * 3)
                                           g = bs `BS.index` (i * 3 + 1)
                                           b = bs `BS.index` (i * 3 + 2)
                                     ]
  where
    pxCnt = BS.length bs `div` 3

imgProperty :: Image3 -> IO ()
imgProperty Image3 { .. } = do
  let header = Header { hMagic = matchBytes
                      , hWidth = fromIntegral iWidth
                      , hHeight = fromIntegral iHeight
                      , hChannels = 3
                      , hColorspace = 0
                      }
  let encoded = encodeRaw header (bytes iBytes) 0
      decoded = decodeQoi $ BS.pack $ A.elems encoded
  decoded `shouldSatisfy` isRight
  case decoded of
       Left _ -> pure ()
       Right (header', Pixels3 pixels') -> do
         header' `shouldBe` header
         pixels' `shouldBe` toArray3 (bytes iBytes)
       Right _ -> fail "Expected Pixels3"

main :: IO ()
main = hspec $ modifyMaxSuccess (const 100000) $
  describe "QOI encoder" $
    parallel $ forM_ ([1..10] :: [Int]) $ \wrk ->
      it ("decode . encode = id (worker " <> show wrk <> ")") $ property imgProperty
