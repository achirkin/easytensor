module Spec (tests) where

import           Distribution.TestSuite

import qualified Numeric.DataFrame.BasicTest
import qualified Numeric.DataFrame.SubSpaceTest

tests :: IO [Test]
tests = return
  [ test "DataFrame.Basic"    Numeric.DataFrame.BasicTest.runTests
  , test "DataFrame.SubSpace" Numeric.DataFrame.SubSpaceTest.runTests
  ]


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
