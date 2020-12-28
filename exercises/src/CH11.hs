module CH11 where

import           Control.Monad.State
import           Control.Monad.Trans.Maybe

type Name = String

data Expr
  = Literal Integer
  | Var Name
  | Op Op Expr Expr

data Op
  = Add
  | Subtract
  | Multiply
  | Divide

type Assignment = [(Name, Integer)]

-- Exercise 11.1
eval :: Expr -> MaybeT (State Assignment) Integer
eval = undefined

