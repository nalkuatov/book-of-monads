{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CH13 where

import           Control.Monad.State
import Control.Exception
import           Data.Map            (Map)
import qualified Data.Map as Map
import qualified System.IO as System.IO
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
data Attempt
  = Match { _word :: String }
  | Mismatch deriving (Eq, Show)

class Beauty where
  attempt :: String -> m Attempt

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

emptyBoard = Map.empty

instance TicTacToe (StateT (Player, Board) IO) where
  info pos = do
    board <- gets snd
    pure $ Map.lookup pos board

  -- | We should actually implement a way to find the winner
  take pos = do
    player <- info pos
    case player of
      Just v  -> do
        liftIO $ print $ "already taken by: " <> show v
        pure $ AlreadyTaken v
      Nothing -> do
        let next X = O
            next O = X
        before <- gets fst
        modify \(p, board) ->
          (next p, Map.insert pos p board)
        (after, board) <- get
        case Map.size board of
          9 -> do
            liftIO $ print $ "game ended"
            pure $ GameEnded after
          _ -> do
            liftIO $ print $ "player " <> show before <> " made a move"
            liftIO $ print board
            liftIO $ print $ "second move is for: " <> show after
            pure NextTurn

-- | Exercise 13.4
type MockFilesystem = Map FilePath String

type FSError = String

instance Exception FSError

class FS m where
  writeFile :: FilePath -> String -> m (Either FSError ())
  readFile  :: FilePath -> m (Either FSError String)

instance FS (State MockFilesystem) where
  writeFile path content = do
    map <- get
    modify $ Map.insert path content
    pure   $ Right ()
  readFile path = do
    file <- gets $ Map.lookup path
    pure $ case file of
      Just v  -> Right v
      Nothing -> Left $ "couldn't find file" <> path

instance FS IO where
  writeFile path content =
    (Right <$> System.IO.writeFile path content)
      `catch` \e -> pure $ Left e
  readFile file = (Right <$> System.IO.readFile file)
      `catch` \e -> pure $ Left e

-- | Exercise 13.5
from :: (() -> a) -> a
from = ($ ())

to :: a -> (() -> a)
to a = \_ -> a

