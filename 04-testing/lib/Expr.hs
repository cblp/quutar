{-# LANGUAGE DeriveGeneric #-}

module Expr where

import           GHC.Generics (Generic)

{-
  sin x + 2 x ^ 4

  expr ::= number
        |  varName
-}

data Expr
  = Number Double
  | Var
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Sin Expr
  | Cos Expr
  | Pow Expr Integer
  deriving (Generic, Read, Show)

instance Eq Expr where
  Number x  == Number y
    | isNaN x, isNaN y   = True
    | otherwise          = x == y
  Var       == Var       = True
  Add x1 x2 == Add y1 y2 = x1 == y1 && x2 == y2
  Sub x1 x2 == Sub y1 y2 = x1 == y1 && x2 == y2
  Mul x1 x2 == Mul y1 y2 = x1 == y1 && x2 == y2
  Div x1 x2 == Div y1 y2 = x1 == y1 && x2 == y2
  Pow x1 x2 == Pow y1 y2 = x1 == y1 && x2 == y2
  Sin x     == Sin y     = x == y
  Cos x     == Cos y     = x == y
  _         == _         = False

showExpr :: Expr -> String
showExpr expr =
  case expr of
    Number n -> show n
    Var      -> "x"
    Sin e    -> "sin " ++ pShow 4 e
    Cos e    -> "cos " ++ pShow 4 e
    Pow e n  -> pShow 3 e ++ " ^ " ++ show n
    Mul a b  -> pShow 1 a ++ " * " ++ pShow 2 b
    Div a b  -> pShow 1 a ++ " / " ++ pShow 2 b
    Add a b  -> pShow 0 a ++ " + " ++ pShow 1 b
    Sub a b  -> pShow 0 a ++ " - " ++ pShow 1 b
  where

    pShow n e
      | prio e <= n = "(" ++ showExpr e ++ ")"
      | otherwise   = showExpr e

    prio :: Expr -> Int
    prio e =
      case e of
        Number{} -> 5
        Var      -> 5
        Sin{}    -> 4
        Cos{}    -> 4
        Pow{}    -> 3
        Mul{}    -> 2
        Div{}    -> 2
        Add{}    -> 1
        Sub{}    -> 1

showFunction :: Expr -> String
showFunction f = "f x = " ++ showExpr f

simplify :: Expr -> Expr
simplify expr =
  case expr of
    Add (Number a) (Number b) -> Number $ a + b
    Add e1 e2 ->
      let e' = Add (simplify e1) (simplify e2)
      in if expr == e' then expr else simplify e'
    _ -> expr

eval :: Expr -> Double -> Double
eval expr x =
  case expr of
    Number n -> n
    Var      -> x
    Add a b  -> eval a x + eval b x
    Sub a b  -> eval a x - eval b x
    Mul a b  -> eval a x * eval b x
    Div a b  -> eval a x / eval b x
    Sin e    -> sin $ eval e x
    Cos e    -> cos $ eval e x
    Pow a n  -> eval a x ^^ n
