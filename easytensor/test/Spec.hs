module Main (tests, main) where

import Distribution.TestSuite
import System.Exit

import qualified Numeric.DataFrame.BasicTest
import qualified Numeric.DataFrame.SubSpaceTest
import qualified Numeric.MatrixDoubleTest
import qualified Numeric.MatrixFloatTest
import qualified Numeric.QuaterDoubleTest
import qualified Numeric.QuaterFloatTest


-- | Collection of tests in detailed-0.9 format
tests :: IO [Test]
tests = return
  [ test "DataFrame.Basic"    $ Numeric.DataFrame.BasicTest.runTests n
  , test "DataFrame.SubSpace" $ Numeric.DataFrame.SubSpaceTest.runTests n
  , test "MatrixDouble"       $ Numeric.MatrixDoubleTest.runTests n
  , test "MatrixFloat"        $ Numeric.MatrixFloatTest.runTests n
  , test "QuaterDouble"       $ Numeric.QuaterDoubleTest.runTests n
  , test "QuaterFloat"        $ Numeric.QuaterFloatTest.runTests n
  ]
  where
    n = 1000 :: Int




-- | Run tests as exitcode-stdio-1.0
main :: IO ()
main = do
    putStrLn ""
    ts <- tests
    trs <- mapM (\(Test ti) -> (,) (name ti) <$> run ti) ts
    case filter (not . isGood . snd) trs of
       [] -> exitSuccess
       xs -> do
        putStrLn $ "Failed tests: " ++ unwords (fmap fst xs)
        exitFailure
  where
    isGood (Finished Pass) = True
    isGood _               = False


-- | Convert QuickCheck props into Cabal tests
test :: String -> IO Bool -> Test
test tName propOp = Test testI
  where
    testI = TestInstance
        { run = fromBool <$> propOp
        , name = tName
        , tags = []
        , options = []
        , setOption = \_ _ -> Right testI
        }
    fromBool False = Finished (Fail "Property does not hold!")
    fromBool True  = Finished Pass
