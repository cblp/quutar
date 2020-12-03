# Composition

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

```plantuml
left to right direction

interface " " as a

rectangle "a → c" {
  rectangle "a → b" as a_b
  rectangle "b → c" as b_c
  a_b --> b_c : b
}

a --> a_b : a

interface " " as c

b_c --> c : c
```

```haskell
writeFile "foo" (show int)

==

(writeFile "foo" . show) int

==

writeInt = writeFile "foo" . show

main = writeInt int
```

# Functor

```haskell
fmap :: (a -> b) -> m a -> m b
```

```plantuml
left to right direction

cloud "m b" {
  cloud "m a" as m_a
  rectangle "a → b" as a_b
  m_a --> a_b : a
}

interface " " as b
a_b --> b : "b"
```

## Functor IO

```haskell
-- 1
getContents :: IO String

-- 2
lines :: String -> [String]

-- 3
lines <$> getContents :: IO [String]

-- 4
map words <$> lines <$> getContents :: IO [[String]]

-- 5
map (map read . words) <$> lines <$> getContents
  :: IO [[Integer]]

-- 6
map (map read . words) . lines <$> getContents
  :: IO [[Integer]]
```

# Applicative

```haskell
(<*>) :: m (a -> b) -> m a -> m b
liftA2 :: (a -> b -> c) -> m a -> m b -> m c

liftA2 f ma mb = f <$> a <*> b
mf <*> ma = liftA2 ($) mf ma
```

```plantuml
left to right direction

cloud "m c" {
  cloud "m a" as m_a
  cloud "m b" as m_b
  rectangle "a → b → c" as a_b_c
  m_a --> a_b_c : a
  m_b --> a_b_c : b
}

interface " " as c
a_b_c --> c : "c"
```

## Applicative IO

```haskell
-- 1
readLn :: IO Integer

-- 2
liftA2 (+) readLn readLn :: IO Integer
```

## Applicative Concurrently

```haskell
-- 1
Concurrently :: IO a -> Concurrently a

-- 2
readLnC :: IO Integer
readLnC = Concurrently readLn

-- 3
liftA2 (+) readLnC readLnC :: IO Integer
```

## Applicative SQL

```haskell
-- 1
sqlQuery "SELECT * FROM students" :: SqlQuery [Student]
sqlQuery "SELECT * FROM teachers" :: SqlQuery [Teacher]

-- 2
expensesA :: SqlQuery Money
expensesA =
  liftA2
    (\students teachers ->
      foldMap scholarship students <>
      foldMap salary teachers)
    (sqlQuery "SELECT * FROM students")
    (sqlQuery "SELECT * FROM teachers")

-- 3
expensesM :: SqlQuery Money
expensesM = do
  students <- sqlQuery "SELECT * FROM students"
  teachers <- sqlQuery "SELECT * FROM teachers"
  pure $
    foldMap scholarship students <>
    foldMap salary teachers
```

# Monad

```haskell
(>>=) :: m a -> (a -> m b) -> m b
```

```plantuml
left to right direction

cloud "m b" {
  cloud "m a" as m_a
  cloud "m b" as m_b
  m_a --> m_b : a
}

interface " " as b
m_b --> b : "b"
```

## Applicative IO

## Monad IO
