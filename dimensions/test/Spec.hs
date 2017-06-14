module Spec (tests) where

import           Distribution.TestSuite

import qualified Numeric.Dimensions.ListTest

tests :: IO [Test]
tests = return
  [ test "Dimensions.List"    Numeric.Dimensions.ListTest.runTests
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
