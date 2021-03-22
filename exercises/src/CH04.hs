module CH04 where

-- | Exercise 4.1
-- | What goes wrong with filterM is that `filter`-s lambda
-- | function's parameter (Bool) is not polymorphic
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f a = sequence . zipWith f a

replicateM :: Monad m => Int -> m a -> m [a]
replicateM x = sequence . replicate x

