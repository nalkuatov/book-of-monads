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

