module Experiments where

import           Prelude                hiding (take)

import           CH13
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State

failList :: [Int]
failList = do
  (x : _) <- []
  return x

-- | Ignore the result type contained in the monad
void_ :: Functor f => f a -> f ()
void_ = fmap $ const ()

ignore :: IO ()
ignore =
  void_ $
  forM ["computers", "are", "the", "root", "of", "evil"] print

play :: (Monad m, TicTacToe m) => m (Maybe Player)
play = do
  take $ Position One One
  take $ Position One Two
  take $ Position One Three
  info $ Position One Two

start :: IO ()
start = do
  _ <- runStateT (play :: StateT (Player, Board) IO (Maybe Player)) (X, emptyBoard)
  pure ()

