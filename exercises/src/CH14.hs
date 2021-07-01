{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module CH14 where

import           CH13 (FSF (..), Free (..))

-- | Exercise 14.1
data (f :+: g) a = InL (f a) | InR (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (InL f') = InL $ fmap f f'
  fmap f (InR g)  = InR $ fmap f g

data RandomGenF r = Next Int Int (Int -> r)

class (f :<: g) where
  inject :: f a -> g a

randomWrite :: (Functor f, FSF :<: f, RandomGenF :<: f)
  => Int -> Int -> Free f a
randomWrite = undefined

