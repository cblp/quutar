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

-- find :: [a] -> a -> Maybe Int

-- C++

-- int i = str.find('x');
-- -- if (i == -1)
-- --     print("not found");
-- -- else
--     use(str[i]);

data Maybe a = Nothing | Just a
