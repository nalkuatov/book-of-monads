module Experiments where

import Control.Monad

failList ∷ [Int]
failList = do
  (x : _) ← []
  return x

-- | Ignore the result type contained in the monad
void_ ∷ Functor f ⇒ f a → f ()
void_ = fmap $ const ()

ignore ∷ IO ()
ignore =
  void_ $
  forM ["computers", "are", "the", "root", "of", "evil"] print

-- | Experiments on `Reader r a` from chapter 6
newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader x) = Reader $ \r -> f $ x r

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x
  Reader f <*> Reader x =
    Reader $ \r -> let a = x r in f r a

instance Monad (Reader r) where
  Reader a >>= f =
    Reader $ \r ->
      let mb = f $ a r in runReader mb r

