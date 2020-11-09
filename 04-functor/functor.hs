import           Data.Map (Map)
import qualified Data.Map as Map

square x = x * x

main :: IO ()
main = do
  print 42
  print 43

data Unit = Unit

-- data () = ()

data Foo = Foo

data Bool = False | True

---------------------------------------------------------------------

-- f :: a -> b

-- (+ 1) :: Integer -> Integer

-- data Maybe a = Nothing | Just a

-- fm :: a -> Maybe b

recipM :: Double -> Maybe Double
recipM 0 = Nothing
recipM x = Just $ 1 / x

-- 42.42 :: Double

-- Double :: Type

-- Maybe Double :: Type

-- Maybe :: Type -> Type -- функция на типах, морфизм

-- template <typename A>
-- class optional...

-- data Either a b = Left a | Right b

recipE :: Double -> Either String Double
recipE 0 = Left "no reciprocal for zero"
recipE x = Right $ 1 / x

-- div :: Integer ->  Integer -> Integer
-- div :: Integer -> (Integer -> Integer)
-- div 48          :: Integer -> Integer -- используем каррирование

-- Either String Double :: Type
-- Either String :: Type -> Type -- используем каррирование
-- Either :: Type -> Type -> Type

-- fm ::
--   a -> Maybe b
--   ==
--   a -> f b
--     where f = Maybe

-- Either e b = (Either e) b

-- fe ::
--   a -> Either e b
--   ==
--   a -> f b
--     where f = Either e

-- data [] a = [] | a : [a]
-- data [a]  = [] | a : [a]

dice :: Integer -> [Integer]
dice n = [1..n]

-- fl ::
--   a -> [b]
--   ==
--   a -> f b
--     where f = []

-- main :: IO ()
-- main = print ...

-- print 84 :: IO ()     :: Type
-- getLine  :: IO String :: Type
-- readLn   :: IO Int    :: Type

-- IO :: Type -> Type

-- print :: Int -> IO ()

-- fi ::
--   a -> IO b
--   ==
--   a -> f b
--     where f = IO

-- hello :: String -> IO String
-- hello myName = do
--   putStrLn $ "Hi! My name is " ++ myName ++ ". What's your name?"
--   getLine

-- a -> f b

-- | "bind", "байнд"
-- (>>=) :: m a -> (a -> m b) -> m b

type Config = Map String Int

data Error = Error{location :: String, description :: String}
  deriving (Show)

getConfig :: IO Config
getConfig =
  -- readFile "config" ...
  pure $ Map.fromList [("port", 109)]

-- getPort :: Config -> Maybe Int
-- getPort config = do
--   case Map.lookup "port" config of
--     Just port -> Just port
--     Nothing   -> Nothing

getPort :: Config -> IO Int
getPort config = do
  case Map.lookup "port" config of
    Just port -> do
      -- checkAvailable port
      pure port
    Nothing   ->
      fail "no port in config"

data User = User{login :: String, uid :: Int}
  deriving (Show)

getUser :: Int -> IO User
getUser port = do
  -- getNetworkData...
  -- decodeData...
  pure User{login = "someuser", uid = 109}

-- $> getConfig >>= getPort >>= getUser

(getConfig >>= getPort >>= getUser) :: IO User
==
(do
  config <- getConfig
  port <- getPort config
  getUser port
  ) :: IO User
