{-# LANGUAGE TypeOperators #-}

module CH14 where

-- | Exercise 14.1
data (f :+: g) a = InL (f a) | InR (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (InL f') = InL $ fmap f f'
  fmap f (InR g)  = InR $ fmap f g

