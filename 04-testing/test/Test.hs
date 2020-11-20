import           Test.Tasty            (defaultMain)
import           Test.Tasty.QuickCheck (conjoin, counterexample, testProperty,
                                        (==>))

-- import           Expr                  ()
import           Square                (solveSquareEquation)

main :: IO ()
main =
  defaultMain $
    testProperty "solveSquareEquation" $
      \a b c -> -- ∀ a b c : Double .
        a /= 0 ==>
          let roots = solveSquareEquation a b c
          in  conjoin -- ∀ x : solveSquareEquation a b c .
                [ -- a x² + b x + x == 0
                  counterexample ("err = " ++ show err) $
                    err < accuracy
                | x <- roots
                , let err =
                        abs (a * x * x + b * x + c)
                        / maximum (map abs [a, b, c])
                ]

accuracy :: Double
accuracy = 1e-9
