{-# LANGUAGE RankNTypes #-}

module CH12 where

import Control.Monad.Identity

--
-- | Exercise 12.1
class Monad m => MonadMn m where
  op :: forall a. m a

