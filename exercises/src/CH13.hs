module CH13 where

import           Prelude hiding (take)

-- | Exercise 13.1: Complete the @Position@ data type below.
-- It represents a position in the 3x3 board used to play tic-tac-toe.
data Position =
  Position { row    :: Tr
           , column :: Tr
           } deriving (Eq, Show)

-- | An additional datatype to define a number of a row or a column
data Tr = One | Two | Three deriving (Eq, Show)

data Player =
  X | O deriving (Eq, Show)

data Result
  = AlreadyTaken { by :: Player }
  | NextTurn
  | GameEnded { winner :: Player }

-- | Exercise 13.2
-- An fictional game shown in a cinematic film
-- about John Nash ("Beautiful Mind").

-- | Exercise 13.3
class TicTacToe m where
  info :: Position -> m (Maybe Player)
  take :: Position -> m Result

takeIfNotTaken
  :: (Monad m, TicTacToe m)
  => Position
  -> m (Maybe Result)
takeIfNotTaken pos = do
  p <- info pos
  case p of
    Just _  -> return Nothing
    Nothing -> Just <$> take pos

