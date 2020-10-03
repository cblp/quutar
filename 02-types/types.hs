module Types where

-- square x = x * x

-- square = \x -> x * x

-- type PointCartesian = (Double, Double)
-- type PointPolar     = (Double, Double)

-- distance :: Point -> Point -> Double
-- distance p1 p2 =
--   let (x1, y1) = p1
--       (x2, y2) = p2
--   in sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- (<--->) :: Point -> Point -> Double
-- p1 <---> p2 =
--   sqrt $ (x1 - x2)^2 + (y1 - y2)^2
--   where
--     (x1, y1) = p1
--     (x2, y2) = p2

-- id :: a -> a
-- id x = x
-- -- alternatively:
-- --  id = \x -> x

-- apply :: (a -> b) -> a -> b
-- (apply f) x = f x
-- -- g = apply f = f :: a -> b
-- -- g x = f x
-- -- g = f

-- apply :: (a -> b) -> (a -> b)
-- apply f = f

-- apply :: (a -> b) -> a -> b
-- apply = id

-- ($) :: (a -> b) -> a -> b
-- ($) = id

data Vector2 = Vector2 Double Double
  deriving (Show)

data Vector3 = Vector3 Double Double Double
  deriving (Show)

-- А распаковывать Vector2 аналогично через let?
-- Vector2 x y = v

-- square x = x * x

-- data Complex a = Complex a a
data Complex a = a :+ a
  deriving (Show, Eq)

-- struct Complex<a> {
--   a x;
--   a y;
-- }

-- | Mass in kilograms
newtype Mass = Kg Double

massFromGrams :: Double -> Mass
massFromGrams g = Kg (g / 1000)

data Point = Point Vector3 Mass

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

-- find :: [a] -> a -> Maybe Int

-- C++

-- int i = str.find('x');
-- -- if (i == -1)
-- --     print("not found");
-- -- else
--     use(str[i]);

data Maybe a = Nothing | Just a
