module CH03 where

-- | Exercise 3.1
-- | check out the results by calling `ap (Just length) (Just "computers")` in an interactive shell
ap ∷ Monad m ⇒ m (b → c) → m b → m c
ap x a = do
  fun   ← x -- unwrap the function b → c from the monadic context
  value ← a -- unwrap the value b
  return $ fun value -- apply the function to the value

-- | Exercise 3.2
fmap_ ∷ Applicative f ⇒ (a → b) -> f a → f b
fmap_ f x = pure f <*> x

-- | Exercise 3.3
newtype ZipList a = ZipList { unZipList ∷ [a] }

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

