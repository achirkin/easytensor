module Main
  ( tests
  , main
  )
where

import           Distribution.TestSuite
import           System.Exit

-- import qualified Data.Type.ListTest
-- import qualified Numeric.Dimensions.DimTest
-- import qualified Numeric.Dimensions.IdxTest
-- import qualified Numeric.Dimensions.Plugin.SolveNat.ExpTest
-- import qualified Numeric.Dimensions.Plugin.SolveNat.ExpStateTest
import qualified Numeric.Dimensions.Plugin.SolveNatTest


-- | Collection of tests in detailed-0.9 format
tests :: IO [Test]
tests =
  return [
  --    test "List"       Data.Type.ListTest.runTests
  -- , test "Dim"        Numeric.Dimensions.DimTest.runTests
  -- , test "Idx"        Numeric.Dimensions.IdxTest.runTests
  -- , 
          test "SolveNat" Numeric.Dimensions.Plugin.SolveNatTest.runTests
  -- , test "Plugin/Exp" Numeric.Dimensions.Plugin.SolveNat.ExpTest.runTests
  -- , test "Plugin/ExpState"
  --        Numeric.Dimensions.Plugin.SolveNat.ExpStateTest.runTests
                                                                         ]




-- | Run tests as exitcode-stdio-1.0
main :: IO ()
main = do
  putStrLn ""
  ts  <- tests
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
      { run       = fromBool <$> propOp
      , name      = tName
      , tags      = []
      , options   = []
      , setOption = \_ _ -> Right testI
      }
    fromBool False = Finished (Fail "Property does not hold!")
    fromBool True  = Finished Pass
