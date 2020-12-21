{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module CH10 where

import           Control.Applicative (Alternative (..), liftA2)

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

