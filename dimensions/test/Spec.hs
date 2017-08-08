module Main (tests, main) where

import           Distribution.TestSuite

import qualified Numeric.Dimensions.ListTest


-- | Collection of tests in detailed-0.9 format
tests :: IO [Test]
tests = return
  [ test "Dimensions.List"    Numeric.Dimensions.ListTest.runTests
  ]




-- | Run tests as exitcode-stdio-1.0
main :: IO Int
main = do
    ts <- tests
    trs <- mapM (\(Test ti) ->(,) (name ti) <$> run ti) ts
    case filter (not . isGood) trs of
       [] -> return 0
       xs -> do
        putStrLn $ "Failed tests: " ++ unwords (fmap fst xs)
        return 1
  where
    isGood (_, Finished Pass) = True
    isGood _ = False


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
