{-# LANGUAGE GADTs #-}

module Experiments where

import           Prelude                hiding (take)

import           CH13                   (Board (..), Player (..), Position (..),
                                         TicTacToe, Tr (..), emptyBoard, info,
                                         take, runRPN)
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

testRPN = runRPN [Number 3, Number 5, Times]

-- | Chapter 13, free applicatives

data Ap f a where
  Pure :: a   -> Ap f a
  Ap   :: f a -> Ap f (a -> b) -> Ap f b

instance Functor (Ap f) where
  fmap f (Pure a) = Pure $ f a

