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
eval =
  \case
    Literal n -> pure n
    Var  name -> MaybeT $ do
      assignment <- get
      pure $ lookup name assignment
    Operation op first second -> do
      x <- eval first
      y <- eval second
      case op of
        Add      -> pure $ x + y
        Subtract -> pure $ x - y
        Multiply -> pure $ x * y
        Divide   ->
          if y == 0 then MaybeT $ pure Nothing
          else pure $ x `div` y

withStateMaybe :: (b -> Maybe a) -> MaybeT (State b) a
withStateMaybe f = MaybeT $ gets f
