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
eval = undefined

