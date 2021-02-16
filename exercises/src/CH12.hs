{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module CH12 where

import           Control.Monad.Identity
import           Control.Monad.Reader

-- | Exercise 12.1
class Monad m => MonadReader' r m | m -> r where
  ask'   :: m r
  reader :: (r -> a) -> m a
  reader f = f <$> ask'

class (Monoid w, Monad m) => MonadWriter' w m | m -> w where
  tell' :: w -> m ()

class Monad m => MonadError' e m | m -> e where
  throwError' :: e -> m a
  catchError' :: m a -> (e -> m a) -> m a

instance Monad m => MonadReader' r (ReaderT r m) where
  ask' = ReaderT pure

instance (MonadWriter' w m) => MonadWriter' w (ReaderT r m) where
  tell' = ReaderT . const . tell'

