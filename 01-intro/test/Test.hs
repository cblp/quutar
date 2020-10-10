import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (assertBool, testCase, (@?=))

import           Square           (solveSquareEquation)

main =
  defaultMain $
    testGroup
      "solveSquareEquation"
      [ testCase "1 2 1" $
          solveSquareEquation 1 2 1 @?= [-1, -1]
      , testCase "1 (-2) 1" $
          solveSquareEquation 1 (-2) 1 @?= [1, 1]
      ]
