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
-- 1
writeFile "foo" (show int)

-- 2
(writeFile "foo" . show) int

-- 3
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

```plantuml
cloud ":: IO [String]" {
  cloud "getContents :: IO String" as getContents

  rectangle "lines :: String → [String]" as lines

  rectangle "map words :: [String] → [ [String] ]" as map_words

  getContents --> lines
  lines --> map_words
}

interface ":: [ [String] ]" as r

map_words --> r
```

```haskell
-- ассоциативность
(a <> b) <> c == a <> (b <> c)

-- ассоциативность
(a . b) . c == a . (b . c)

-- "ассоциативность"
(f . g) <$> ma == f <$> (g <$> ma)

-- fmap (f . g) ma == fmap f (fmap g ma)
-- (fmap (f . g)) ma == (fmap f . fmap g) ma

fmap (f . g) == fmap f . fmap g
```

# Applicative

```haskell
(<*>) :: m (a -> b) -> m a -> m b
liftA2 :: (a -> b -> c) -> m a -> m b -> m c
```

<!-- ```haskell
liftA2 f ma mb = f <$> a <*> b
mf <*> ma = liftA2 ($) mf ma
``` -->

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
liftA2 (-) readLn readLn :: IO Integer
```

## Applicative Concurrently

```haskell
-- 4
Concurrently :: IO a -> Concurrently a

-- 5
readLnC :: Concurrently Integer
readLnC = Concurrently readLn

-- 6
liftA2 (-) readLnC readLnC :: Concurrently Integer
```

## Applicative SQL

```haskell
-- 1
sqlQuery "SELECT * FROM students" :: SqlQuery [Student]
sqlQuery "SELECT * FROM teachers" :: SqlQuery [Teacher]

-- 2
expensesParallel :: SqlQuery Money
expensesParallel =
  liftA2
    (\students teachers ->
      foldMap scholarship students <>
      foldMap salary teachers)
    (sqlQuery "SELECT * FROM students")
    (sqlQuery "SELECT * FROM teachers")

-- 3
expensesSequential :: SqlQuery Money
expensesSequential = do
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

```haskell
-- 1
expenses1 :: SqlQuery Money
expenses1 = do
  students <- sqlQuery "SELECT * FROM students"
  teachers <- sqlQuery "SELECT * FROM teachers"
  pure $
    foldMap scholarship students <>
    foldMap salary teachers

-- 2
expenses2 :: SqlQuery Money
expenses2 =
  sqlQuery "SELECT * FROM students"
  >>=
  (\students ->
      sqlQuery "SELECT * FROM teachers"
      >>=
      (\teachers ->
          pure $
            foldMap scholarship students <>
            foldMap salary teachers))
```

## Megaparsec

```haskell
-- parser for decimal number
float :: Megeparsec.Parser Double
float =
  liftA2
    stringsToFloat
    (some digit)
    (optional $ char '.' *> some digit)
  where
    stringsToFloat int mfrac =
      fromIntegral (stringToInt int) +
      maybe
        0
        (\frac ->
            fromIntegral (stringToInt frac)
            / 10 ^ length frac)
        mfrac
```

## optparse-applicative

```haskell
data Options = Options
  { verbose    :: Bool
  , inputFile  :: FilePath
  , outputFile :: Maybe FilePath
  }

options :: Optparse.Parser Options
options = do
  verbose <-
    switch $ long "verbose" <> short 'v' <> help "Print debug information"
  inputFile <-
    strOption $ long "input" <> short 'i' <> metavar "FILE" <> help "Input file"
  outputFile <-
    optional $
    strOption $
      long "output" <>
      short 'o' <>
      metavar "FILE" <>
      help "Output file, default is standard output"
  pure Options{..}
```

```
$ myprog --help

Usage: myprog [-v|--verbose] (-i|--input=FILE) [-o|--output=FILE]

Available options:
  -v,--verbose      Print debug information
  -i,--input=FILE   Input file
  -o,--output=FILE  Output file, default is standard output
```

## Регулярные выражения

`[0-9]+(\.[0-9]+)?`

## regex-posix

`"3.14" =~ "[0-9]+(\\.[0-9]+)?"`

## regex-applicative

```haskell
-- parser for decimal number
float :: RE Char Double
float =
  liftA2
    stringsToFloat
    (some digit)
    (optional $ char '.' *> some digit)
  where
    stringsToFloat int mfrac =
      fromIntegral (stringToInt int) +
      maybe
        0
        (\frac ->
            fromIntegral (stringToInt frac)
            / 10 ^ length frac)
        mfrac
```

# Валидация

https://github.com/system-f/validation/blob/master/examples/src/Email.hs

# Haxl (Facebook Sigma)

https://github.com/facebook/Haxl/tree/master/example/sql

```java
-- 1
NumCommonFriends(x, y) = Length(Intersect(FriendsOf(x), FriendsOf(y)))
```

```haskell
-- 2
numCommonFriends x y =
  length <$> (intersect <$> friendsOf x <*> friendsOf y)

-- 3
friendsOfFriends :: Id -> Haxl [Id]
friendsOfFriends id = do
  friends <- friendsOf id
  fofs <- mapM friendsOf friends
  return (concat fofs)
```
