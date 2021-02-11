{-# LANGUAGE RankNTypes #-}

module CH12 where

import Control.Monad.Identity

--
-- | Exercise 12.1
class Monad m => MonadReader' r m | m -> r where
  ask'   :: m r
  reader :: (r -> a) -> m a
  reader f = f <$> ask'

class (Monoid w, Monad m) => MonadWriter' w m | m -> w where
  tell :: w -> m ()

