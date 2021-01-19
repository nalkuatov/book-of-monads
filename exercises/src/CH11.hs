{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module CH11 where

import           Control.Monad.Cont
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           System.IO                  (IO)

type Name = String

data Expr
  = Literal Integer
  | Var Name
  | Operation Op Expr Expr -- ^ Change the constructor name from `Op` (as written in the book) to `Operation` to
    -- avoid the confusion with the `Op` data type below

data Op
  = Add
  | Subtract
  | Multiply
  | Divide

type Assignment = [(Name, Integer)]

-- Exercise 11.1
eval :: Expr -> MaybeT (State Assignment) Integer
eval (Literal n) = pure n
eval (Var name) = withStateMaybe $ lookup name
eval (Operation op x y) = do
  x' <- eval x
  y' <- eval y
  case op of
    Add      -> pure $ x' + y'
    Subtract -> pure $ x' - y'
    Multiply -> pure $ x' * y'
    Divide   -> withStateMaybe $ const $
      if (y' /= 0)
        then Just $ div x' y'
        else Nothing

withStateMaybe :: (b -> Maybe a) -> MaybeT (State b) a
withStateMaybe = MaybeT . gets

-- Exercise 11.2
instance {-# Overlaps #-} Monad (MaybeT (Reader r)) where
  return = pure
  MaybeT rma >>= f = MaybeT $ do
    ma <- rma
    case ma of
      Just a  -> runMaybeT $ f a
      Nothing -> return $ Nothing

instance {-# Overlaps #-} Monad (ExceptT e (Reader r)) where
  return = pure
  ExceptT rma >>= f = ExceptT $ do
    ma <- rma
    case ma of
      Left  e -> return $ Left e
      Right v -> runExceptT $ f v

instance {-# Overlaps #-} Monad (StateT s Maybe) where
  return = pure
  StateT smas >>= f = StateT $ \s -> do
    (a, s') <- smas s
    runStateT (f a) s'

instance {-# Overlaps #-} Monad (ReaderT r IO) where
  return a = ReaderT $ \_ -> pure a
  ReaderT rma >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

instance {-# Overlaps #-} Monoid w => Monad (WriterT w IO) where
  return a = WriterT $ pure (a, mempty)
  WriterT maw >>= f = WriterT $ do
    (a, w)  <- maw
    (b, w') <- runWriterT (f a)
    pure $ (b, w <> w')

instance {-# Overlaps #-} Monad (ContT r IO) where
  return a = ContT ($ a)
  (ContT amrmr) >>= f =
     ContT $ \bmr -> amrmr $ flip (runContT . f) bmr

