{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CH13 where

import           Control.Monad.State
import           Data.Map            (Map, insert, lookup, empty)
import           Prelude             hiding (lookup, take)

-- | Exercise 13.1: Complete the @Position@ data type below.
-- It represents a position in the 3x3 board used to play tic-tac-toe.
data Position =
  Position { row    :: Tr
           , column :: Tr
           } deriving (Eq, Show, Ord)

-- | An additional datatype to define a number of a row or a column
data Tr = One | Two | Three deriving (Eq, Show, Ord)

data Player =
  X | O deriving (Eq, Show)

data Result
  = AlreadyTaken { by :: Player }
  | NextTurn
  | GameEnded { winner :: Player }
  deriving (Eq, Show)

-- | Exercise 13.2
-- A fictional game shown in a cinematic film
-- about John Nash ("Beautiful Mind").

-- | Exercise 13.3
takeIfNotTaken
  :: (Monad m, TicTacToe m)
  => Position
  -> m (Maybe Result)
takeIfNotTaken pos = do
  p <- info pos
  case p of
    Just _  -> return Nothing
    Nothing -> Just <$> take pos

class TicTacToe m where
  info :: Position -> m (Maybe Player)
  take :: Position -> m Result

type Board = Map Position Player

emptyBoard = empty

instance TicTacToe (State (Player, Board)) where
  info pos = do
    (_, board) <- get
    pure $ lookup pos board
  take pos = do
    player <- info pos
    case player of
      Just v  -> pure $ AlreadyTaken v
      Nothing -> do
        let next X = O
            next O = X
        modify \(p, board) ->
          (next p, insert pos p board)
        pure NextTurn

-- | Exercise 13.4

