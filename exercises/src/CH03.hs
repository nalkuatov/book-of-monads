module CH03 where

-- | Exercise 3.1
-- | check out the results by calling `ap (Just length) (Just "computers")` in an interactive shell
ap ∷ Monad m ⇒ m (b → c) → m b → m c
ap x a = do
  fun   ← x -- unwrap the function b → c from the monadic context
  value ← a -- unwrap the value b
  return $ fun value -- apply the function to the value

