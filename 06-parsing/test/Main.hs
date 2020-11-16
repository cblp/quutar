{-# OPTIONS -Wno-orphans #-}

import           Test.Tasty            (TestName, TestTree, defaultMain,
                                        testGroup)
import           Test.Tasty.HUnit      (assertFailure, testCase, (@?=))
import           Test.Tasty.QuickCheck (Arbitrary, arbitrary, getSize, oneof,
                                        -- counterexample, testProperty, (===),
                                        scale)
import           Text.Megaparsec       (errorBundlePretty, parse)

import           Expr                  (Expr (..),
                                        -- showExpr,
                                        )
import           Expr.Parse            (expr)

main :: IO ()
main =
  defaultMain $
    testGroup "parsing"
      [ caseParseExpr "positive integer" "4"     $ Number 4
      , caseParseExpr "negative integer" "-4"    $ Number (-4)
      , caseParseExpr "floating"         "-4.18" $ Number (-4.18)
      , caseParseExpr "var"              "x"       Var
      -- , testProperty "show <-> parse" $
      --   \e ->
      --     case parse expr "show" (showExpr e) of
      --       Left errorBundle ->
      --         counterexample (errorBundlePretty errorBundle) False
      --       Right e' -> e' === e
      ]

caseParseExpr :: TestName -> String -> Expr -> TestTree
caseParseExpr description input expected =
  testCase description $
    case parse expr "input" input of
      Left errorBundle -> assertFailure $ errorBundlePretty errorBundle
      Right e          -> e @?= expected

instance Arbitrary Expr where
  arbitrary = do
    size <- getSize
    oneof $ simple ++ if size >= 1 then complex else []
    where
      halve = scale (`div` 2)
      simple =
        [ pure Var
        , Number <$> oneof [arbitrary, fromInteger <$> arbitrary]
        ]
      complex =
        [ Sin <$> arbitrary
        , Cos <$> arbitrary
        , Pow <$> halve arbitrary <*> halve arbitrary
        , Mul <$> halve arbitrary <*> halve arbitrary
        , Div <$> halve arbitrary <*> halve arbitrary
        , Add <$> halve arbitrary <*> halve arbitrary
        , Sub <$> halve arbitrary <*> halve arbitrary
        ]
