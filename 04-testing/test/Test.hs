{-# OPTIONS -Wno-orphans #-}

import           Test.Tasty            (defaultMain)
import           Test.Tasty.QuickCheck (Arbitrary, arbitrary, conjoin,
                                        counterexample, getSize, oneof, resize,
                                        testProperty, (==>))

import           Expr                  (Expr (..))
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
accuracy = 1e-12

instance Arbitrary Expr where
  arbitrary = do
    size <- getSize
    let halve = resize (size `div` 2)
    oneof
      $   [ pure Var
          , Number <$> oneof [arbitrary, fromInteger <$> arbitrary]
          , Sin <$> arbitrary
          , Cos <$> arbitrary
          , Pow <$> halve arbitrary <*> halve arbitrary
          ]
      ++  if size >= 1 then
            [ Mul <$> halve arbitrary <*> halve arbitrary
            , Div <$> halve arbitrary <*> halve arbitrary
            , Add <$> halve arbitrary <*> halve arbitrary
            , Sub <$> halve arbitrary <*> halve arbitrary
            ]
          else
            []
