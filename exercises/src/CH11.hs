{-# LANGUAGE LambdaCase #-}

module CH11 where

import           Control.Monad.State
import           Control.Monad.Trans.Maybe

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
