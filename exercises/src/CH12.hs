{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module CH12 where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.ST
import           System.IO

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

-- One more instance for MonadError' should be here.
-- I am skipping this for now.

-- | Exercise 12.2
class (Monad b, Monad m) => MonadBase b m | m -> b where
  liftBase :: b a -> m a

instance MonadBase IO IO where
  liftBase = id

instance MonadBase (ST s) (ST s) where
  liftBase = id

instance (Monad (t m), MonadBase b m, MonadTrans t)
       => MonadBase b (t m) where
  liftBase = lift . liftBase

-- | Exercise 12.3
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile = undefined

