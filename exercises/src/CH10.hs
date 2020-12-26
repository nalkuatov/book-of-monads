{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE InstanceSigs        #-}

module CH10 where

import           Control.Applicative    (Alternative (..), liftA2)

newtype (f :.: g) a = Compose (f (g a))

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Compose x) = Compose $ fmap (fmap f) x

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure x = Compose $ pure $ pure x
  Compose f <*> Compose x =
    Compose $ liftA2 (<*>) f x

instance (Alternative f, Applicative g) => Alternative (f :.: g) where
  empty = Compose empty
  Compose f <|> Compose g = Compose (f <|> g)

-- Exercise 10.1

newtype Writer w a =
  Writer { unWriter :: (a, w)
         }

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  Writer (f, w1) <*> Writer (a, w2) =
    Writer (f a, w1 <> w2)

instance Monoid w => Monad (Writer w) where
  return = pure
  Writer (a, w) >>= f =
    Writer $ fmap (w <>) . unWriter . f $ a

class (Monad m, Monad n) => Swappable m n where
  swap :: m (n a) -> n (m a)

instance (Monad m, Monoid w) => Swappable (Writer w) m where
  -- swap :: Monad m => (Writer w :.: m) a -> (m :.: Writer w) a
  -- The signature of `swap` above is equivalent to the one below
  -- but requires additional unwrapping using `Compose` constructor
  swap :: Writer w (m a) -> m (Writer w a)
  swap (Writer (ma, w)) = fmap (Writer . (, w) ) $ ma

newtype Reader r a =
  Reader { unReader :: r -> a
         }

instance Functor (Reader r) where
  fmap f (Reader x) = Reader $ \r -> f (x r)

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x
  Reader f <*> Reader x =
    Reader $ \r -> f r $ x r

instance Monad (Reader r) where
  return = pure
  Reader x >>= f =
    Reader $ \r -> ($ r) . unReader . f $ x r

instance Monad m => Swappable m (Reader r) where
  swap :: Monad m => m (Reader r a) -> Reader r (m a)
  swap mra = Reader $ \r -> fmap (($ r) . unReader) mra

-- Seems like there's no way to implement the function below.
-- We should figure out the reason soon.
instance Monad m => Swappable (Reader r) m where
  swap :: Monad m => Reader r (m a) -> m (Reader r a)
  swap _ = undefined

-- Exercise 10.2

newtype Listed m a =
  Listed { unListed :: [m a]
         }

instance Functor m => Functor (Listed m) where
  fmap f (Listed xs) = Listed $ fmap (fmap f) xs

instance Applicative m => Applicative (Listed m) where
  pure x = Listed [pure x]
  Listed fs <*> Listed xs =
    Listed $ fmap (<*>) fs <*> xs

