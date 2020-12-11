module CH08 where

import           Control.Concurrent.STM.TVar
import           Control.Monad               (guard)
import           Control.Monad.STM

-- Exercise 8.1
_addName
  :: TVar Integer
  -> TVar [(Integer, String)]
  -> String
  -> STM ()
_addName counter names name = do
  i      <- readTVar counter
  values <- readTVar names
  case lookup i values of
    Nothing -> do
      modifyTVar names ((i, name) :)
      writeTVar counter (i + 1)
    Just _ -> pure ()

addName :: String -> STM ()
addName name = do
  counter <- newTVar 0
  names   <- newTVar []
  _addName counter names name

