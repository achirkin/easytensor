import           Control.Monad               (unless)
import qualified Numeric.Dimensions.ListTest

main :: IO ()
main =
  test "Dimensions.List"    Numeric.Dimensions.ListTest.runTests



test :: String -> IO Bool -> IO ()
test name t = t >>= flip unless (putStrLn $ name ++ ": failure!")
