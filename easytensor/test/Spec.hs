import           Control.Monad                  (unless)

import qualified Numeric.DataFrame.BasicTest
import qualified Numeric.DataFrame.SubSpaceTest

main :: IO ()
main = do
  test "DataFrame.Basic"    Numeric.DataFrame.BasicTest.runTests
  test "DataFrame.SubSpace" Numeric.DataFrame.SubSpaceTest.runTests



test :: String -> IO Bool -> IO ()
test name t = t >>= flip unless (putStrLn $ name ++ ": failure!")
