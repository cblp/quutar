module Expr where

{-
  sin x + 2 x ^ 4

  expr ::= number
        |  varName
-}

-- -- k x + b
-- data Expr = Expr Double -- k
--                  Double -- b

-- -- struct/class
-- data Expr = Expr ExprType Double Char

-- -- enum
-- data ExprType = Number | Var

-- sum type
data Expr
  = Number Double
  | Var
  | Plus Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Sin Expr
  | Cos Expr
  | Pow Expr Integer
  deriving (Eq)

instance Show Expr where
  show expr =
    case expr of
      Number n   -> show n
      Var        -> "x"
      Plus e1 e2 -> "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
      Mul  e1 e2 -> "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
      Sub  e1 e2 -> "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
      Div  e1 e2 -> "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
      Sin e      -> "(sin " ++ show e ++ ")"
      Cos e      -> "(cos " ++ show e ++ ")"
      Pow e n    -> "(" ++ show e ++ " ^ " ++ show n ++ ")"

showFullExpr :: Expr -> String
showFullExpr f = "f x = " ++ show f

simplify :: Expr -> Expr
simplify expr =
  case expr of
    Plus (Number a) (Number b) -> Number $ a + b
    Plus e1 e2 ->
      let e' = Plus (simplify e1) (simplify e2)
      in if expr == e' then expr else simplify e'
    _ -> expr

eval :: Expr -> Double -> Double
eval expr x =
  case expr of
    Plus expr1 expr2 -> eval expr1 x + eval expr2 x
    Number n         -> n
    Var              -> x

    -- вот это надо убрать в соответствующем варианте:
    _                -> undefined
