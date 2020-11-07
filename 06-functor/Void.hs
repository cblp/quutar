{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyCase #-}

-- C:           void f(void) {}
-- Haskell:     f :: () -> IO () = \() -> pure ()

-- data () = ()

data Void a

instance Functor Void where
  fmap :: (a -> b) -> Void a -> Void b
  fmap f v =
    case v of
      -- no branches
