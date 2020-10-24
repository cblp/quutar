import           Test.Tasty            (defaultMain, testGroup)
import           Test.Tasty.HUnit      (assertBool, testCase, (@?=))
import           Test.Tasty.QuickCheck (conjoin, counterexample, label,
                                        testProperty, (===), (==>))

import           Square                (solveSquareEquation)

main :: IO ()
main =
  defaultMain $
    testGroup "solveSquareEquation"
      [ testProperty "any a, b, c" $
          -- ∀ a b c : Double .
          \a b c ->
            a /= 0 ==>
              let roots = solveSquareEquation a b c
              in  -- ∀ x : solveSquareEquation a b c .
                  --   a x² + b x + x == 0
                  conjoin
                    [ counterexample ("err = " ++ show err) $
                        err < accuracy
                    | x <- roots
                    , let err =
                            (a * x * x + b * x + c)
                            / maximum (map abs [a, b, c])
                    ]
      ]

accuracy = 1e-12
