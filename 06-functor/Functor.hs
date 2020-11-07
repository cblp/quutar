{-
  Functor
    |
  Applicative
    |     \
  Monad   Alternative
      \   |
      MonadPlus
-}

class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> (f a -> f b)

fmap @[] :: (a -> b) -> [] a -> [] b
fmap @[] :: (a -> b) -> [a] -> [b]
 map     :: (a -> b) -> [a] -> [b]
 map     :: (a -> b) ->([a] -> [b])

map f as = bs

map f = \as -> bs

fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b

fmap f ma == do a <- ma
                pure (f a)

data Ignore a = Ignore

instance Functor Ignore where
  fmap :: (a -> b) -> (Ignore a -> Ignore b)
  fmap _ Ignore = Ignore

data Either a b = Left a | Right b

Either a b = (Either a) b

instance Functor (Either c) where
  fmap :: (a -> b) -> (Either c a -> Either c b)

instance Functor IO where
  fmap :: (a -> b) -> (IO a -> IO b)

(<$>) = fmap

---------------------------------------------------------

class Functor f => Applicative f where

  pure :: a -> f a

  -- | ap
  (<*>) :: f (a -> b) -> f a -> f b

-- f = Either c
pure :: a -> Either c a

getF <*> getX
  ==
  do  f <- getF
      x <- getX
      pure (f x)

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

do  () <- getA
    b  <- getB
    pure b

getA *> getB = (\_ b -> b) <$> getA <*> getB
