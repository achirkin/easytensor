import           Control.Monad                  (unless)

import qualified Numeric.DataFrame.BasicTest
import qualified Numeric.DataFrame.SubSpaceTest
import qualified Numeric.Dimensions.ListTest

main :: IO ()
main = do
  test "DataFrame.Basic"    Numeric.DataFrame.BasicTest.runTests
  test "DataFrame.SubSpace" Numeric.DataFrame.SubSpaceTest.runTests
  test "Dimensions.List"    Numeric.Dimensions.ListTest.runTests



test :: String -> IO Bool -> IO ()
test name t = t >>= flip unless (putStrLn $ name ++ ": failure!")
