{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
module Main (main) where


import qualified Data.ByteString as BS
-- import           Data.Word
import           Data.Maybe                      (fromMaybe)
import           Data.Type.Lits
import           Numeric.DataFrame               hiding (D, S)
import qualified Numeric.Dimensions              as Dim
import           OpenCV                          (DS (..), Mat)
import qualified OpenCV                          as OpenCV
import           OpenCV.Core.Types.Mat.DataFrame
import           Paths_easytensor_opencv



main :: IO ()
main = do
  imgPath <- getDataFileName "example/example.png"
  -- Depth becomes KnownDim t => 'S t
  --   this is an essential step before doing anything in easytensor
  FixDepth (t :: PrimTag t) matSC <- fixDepth <$> loadPicture imgPath
  -- Here I find out KnownDim number of channels
  FixChannels (c :: Dim (c :: Nat)) matS <- pure $ fixChannels matSC
  -- And finally, find the dimensionality of the image.
  -- this is not strictly necessary, but makes a lot of things simpler
  FixShape (s :: Dims (ds :: [Nat])) mat <- pure $ fixShape matS

  putStrLn $ "Matrix elem tag: " ++ show t
  putStrLn $ "Matrix channels: " ++ show c
  putStrLn $ "Matrix shape:    " ++ show s

  -- Now, let's try to do something meaningful if this is a 2D RGBA image
  -- Sometimes GHC goes haywire if I pattern match a few GADT things at a time
  -- and complains that a pattern is inaccessible even if it is.
  -- To workaround this, I use Maybe type with its fail instance.
  fromMaybe (putStrLn "This image format was not expected.") $ do
    PTagWord8 <- Just t
    D4 <- Just c
    Dim.D :* Dim.D :* U  <- Just s
    MChMatDF x <- Just $ OpenCV.fromMat @(MChMatDF t ds c) mat
    -- Know we have exact shape of x with all dims known, we can use any functions
    -- from what easytensor can offer.
    let y = ewmap (\(Vec4 r g b a) -> Vec4 b g (r `div` 2) a) x
        z = ewmap (\(Vec4 r g b _) -> scalar (r + g - b)) x
        maty = OpenCV.toMat (MChMatDF y)
        matz = OpenCV.toMat (SChMatDF z)
    return $ do
      savePicture "example-out-color.png" maty
      savePicture "example-out-grey.png" matz
      print "Done!"



loadPicture :: FilePath -> IO (OpenCV.Mat ('S ['D, 'D]) 'D 'D)
loadPicture = fmap (OpenCV.imdecode OpenCV.ImreadUnchanged) . BS.readFile


savePicture :: FilePath -> Mat shape channels depth -> IO ()
savePicture path = BS.writeFile path . encodePicture


encodePicture :: Mat shape channels depth -> BS.ByteString
encodePicture = OpenCV.exceptError
              . OpenCV.imencode (OpenCV.OutputPng OpenCV.defaultPngParams)
