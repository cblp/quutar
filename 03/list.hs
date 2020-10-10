module List where

-- $ > []

-- $ > [3, 15, 9, 20]

-- $ > ['a', 'b', 'c', 'd', 'e', 'f']

-- $ > [10..20]

-- $ > [10, 14 .. 20]

-- $ > ['a', 'b' .. 'f'] -- тоже работает

-- $ > :t (:)

-- $ > 'a' : "bcd"

-- $ > let x : xs = [10..20] in (x, xs)

data Vector2 = Vector2 Double Double

newtype Mass = Kg Double

data Point = Point Vector2 Mass

-- data Str
--   = Cons Char Str
--   | End

{-
  "" = End
  "a" = Cons 'a' End
  "ab" = Cons 'a' $ Cons 'b' End
-}

-- f(x)
-- F[A], F<A>

data List a
  = Cons a (List a)
  | End

{-
  [] = End
  [1] = Cons 1 End
  [a, b] = Cons a $ Cons b End
-}

{-
  data [] a
    = a : [a]
    | []

  [] = []
  [1] = 1 : []
  ['a', 'b'] = 'a' : 'b' : []
-}

-- $ > :i []

-- $ > :i Maybe

f1 :: Maybe Int -> String
f1 m =
  case m of
    Nothing -> "no numbers"
    Just x  -> "this is " ++ show x

f2 :: [Int] -> String
f2 list =
  case list of
    x : xs -> ""
    []     -> "empty"

-- if (list.isEmpty()) {
--     list.first()...
-- }

-- $ > maxBound :: Char

-- $> [Data.Char.chr i | i <- [32 .. 64]]

-- $> map Data.Char.chr [32 .. 64]

map' :: (a -> b) -> [a] -> [b]
-- map' f list =
--     case list of
--         [] -> []
--         x : xs -> f x : map' f xs
map' _ []       = []
map' f (x : xs) = f x : map' f xs

-- $> [c | c <- [' ' .. '~'], Data.Char.isAlphaNum c]

-- $> filter Data.Char.isAlphaNum [' ' .. '~']

filter' :: (a -> Bool) -> [a] -> [a]
-- filter' p list =
--     case list of
--         [] -> []
--         x : xs ->
--             if p x then x : filter' p xs else filter' p xs

-- filter' p list =
--     case list of
--         []     -> []
--         x : xs
--             | p x       -> x : filter' p xs
--             | otherwise ->     filter' p xs

filter' p [] = []
filter' p (x : xs)
    | p x       = x : filter' p xs
    | otherwise =     filter' p xs

-- $> [Data.Char.ord c | c <- [' ' .. '~'], Data.Char.isAlphaNum c]

-- $> map Data.Char.ord $ filter Data.Char.isAlphaNum [' ' .. '~']
