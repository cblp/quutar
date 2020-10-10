import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Square           (solveSquareEquation)

main :: IO ()
main =
  defaultMain $
    testGroup "solveSquareEquation"
      [ testCase "1 2 1" $
          solveSquareEquation 1 2 1 @?= [-1, -1]
      , testCase "1 (-2) 1" $
          solveSquareEquation 1 (-2) 1 @?= [1, 1]
      ]
