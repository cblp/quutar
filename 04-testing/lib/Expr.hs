module Expr where

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
  deriving (Eq)

instance Show Expr where
  show expr =
    case expr of
      Number n  -> show n
      Var       -> "x"
      Add e1 e2 -> "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
      Mul e1 e2 -> "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
      Sub e1 e2 -> "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
      Div e1 e2 -> "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
      Sin e     -> "(sin " ++ show e ++ ")"
      Cos e     -> "(cos " ++ show e ++ ")"
      Pow e n   -> "(" ++ show e ++ " ^ " ++ show n ++ ")"

showFullExpr :: Expr -> String
showFullExpr f = "f x = " ++ show f

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
