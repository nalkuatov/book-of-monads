{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CH13 where

import           Control.Exception
import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Prelude             hiding (lookup, take)
import qualified System.IO           as System.IO

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

-- | Exercise 13.6
data Tictactoe a
  = Info Position (Maybe Player -> Tictactoe a)
  | Take Position (Result       -> Tictactoe a)
  | Done a

instance Functor Tictactoe where
  fmap f (Done a)     = Done $ f a
  fmap f (Info pos g) = Info pos (fmap f . g)
  fmap f (Take pos g) = Take pos (fmap f . g)

instance Applicative Tictactoe where
  pure    = Done
  Done f     <*> g = fmap f g
  Info pos f <*> g = Info pos $ (<*> g) . f
  Take pos f <*> g = Take pos $ (<*> g) . f

instance Monad Tictactoe where
  return = pure
  Done a     >>= f = f a
  Info pos a >>= f = Info pos (\p -> a p >>= f)
  Take pos a >>= f = Take pos (\p -> a p >>= f)

info' :: Position -> Tictactoe (Maybe Player)
info' p = Info p return

take' :: Position -> Tictactoe Result
take' p = Take p return

takeIfNotTaken'
  :: Position
  -> Tictactoe (Maybe Result)
takeIfNotTaken' pos = do
  p <- info' pos
  case p of
    Just _  -> pure Nothing
    Nothing -> Just <$> take' pos

-- | Exercise 13.8
data Fs a
  = WriteFile FilePath String (Either FSError () -> Fs a)
  | ReadFile  FilePath (Either FSError String    -> Fs a)
  | FSDone a

writeFile' :: FilePath -> String -> Fs (Either FSError ())
writeFile' path content = WriteFile path content FSDone

readFile' :: FilePath -> Fs (Either FSError String)
readFile' path = ReadFile path FSDone

interpret :: Fs a -> State MockFilesystem a
interpret (FSDone a) = pure a
interpret (WriteFile path content f) = do
  modify $ Map.insert path content
  interpret $ f $ Right ()
interpret (ReadFile path f) = do
  content <- Map.lookup path <$> get
  interpret $ f $ case content of
    Just v  -> Right v
    Nothing -> Left "not found"

-- | Exercise 13.9
data TictactoeF r
  = InfoF Position (Maybe Player -> r)
  | TakeF Position (Result       -> r)

instance Functor TictactoeF where
  fmap f (InfoF pos a) = InfoF pos $ f . a
  fmap f (TakeF pos a) = TakeF pos $ f . a

-- | Exercise 13.10
data Free f a = Free (f (Free f a))
              | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free x) = Free $ fmap (fmap f) x

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> x = f <$> x
  Free f <*> x = Free (fmap (<*> x) f)

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free a >>= f = Free (fmap (>>= f) a)

{-
    fmap f (Free (Info p k)) = Free (Info p $ fmap f k)
--  Move fmap outside of Info constructor via another fmap
=== fmap f (Free (Info p k)) = Free $ fmap (fmap f) (Info p k)
--  Abstract Info into a variable
=== fmap f (Free x) = Free $ fmap (fmap f) x
-}

-- | Exercise 13.11
data FSF a
  = WriteFileF FilePath String (Either FSError () -> a)
  | ReadFileF  FilePath (Either FSError String -> a)

interpret' :: FSF a -> State MockFilesystem a
interpret' (ReadFileF path f) = do
  content <- Map.lookup path <$> get
  pure $ f $ case content of
    Just v  -> Right v
    Nothing -> Left "not found"
interpret' (WriteFileF path content f) = do
  modify $ Map.insert path content
  pure $ f $ Right ()

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap return

foldFree :: Monad m => (forall r. f r -> m r) -> Free f a -> m a
foldFree _ (Pure x) = return x
foldFree interpret (Free x) = do
  x' <- interpret x
  foldFree interpret x'

runFS :: Free FSF a -> State MockFilesystem a
runFS = foldFree interpret'

-- | Exercise 13.12
data TictactoeOp a where
  InfoOp :: Position -> TictactoeOp (Maybe Player)
  TakeOp :: Position -> TictactoeOp Result
  Return :: a        -> TictactoeOp a
  Bind   :: TictactoeOp a -> (a -> TictactoeOp b) -> TictactoeOp b

instance Functor TictactoeOp where
  fmap f a = a `Bind` (Return . f)

instance Applicative TictactoeOp where
  pure    = Return
  f <*> a = f `Bind` (\f' -> a `Bind` (Return . f'))

instance Monad TictactoeOp where
  return = Return
  (>>=)  = Bind

-- | Exercise 13.13
data FSOp a where
  ReadFileOp  :: FilePath -> FSOp (Either FSError String)
  WriteFileOp :: FilePath -> String -> FSOp (Either FSError ())
  Return'     :: a        -> FSOp a
  Bind'       :: FSOp a   -> (a -> FSOp b) -> FSOp b

-- | Exercise 13.14
data Program instr a where
  PDone  :: a               -> Program instr a
  PBind  :: Program instr a -> (a -> Program instr b) -> Program instr b
  Instr  :: instr a         -> Program instr a

instance Functor (Program instr) where
  fmap f a = a `PBind` (PDone . f)

instance Applicative (Program instr) where
  pure = PDone
  f <*> a = f `PBind` (\f' -> a `PBind` (PDone . f'))

instance Monad (Program instr) where
  return = PDone
  (>>=)  = PBind

-- | Exercise 13.15
data Freer instr a where
  Pure'  :: a -> Freer instr a
  Impure :: instr a -> (a -> Freer instr b) -> Freer instr b

instance Functor (Freer instr) where
  fmap f (Pure' a) = Pure' $ f a
  fmap f (Impure instr f') =
    Impure instr ((f <$>) . f')

instance Applicative (Freer instr) where
  pure  = Pure'
  Pure' f <*> Pure' x = Pure' $ f x
  Pure' f <*> Impure instr g =
    Impure instr (fmap f . g)
  Impure instr f <*> x =
    Impure instr ((<*> x) . f)

instance Monad (Freer instr) where
  return = pure
  Pure' a        >>= f = f a
  Impure instr a >>= f =
    Impure instr (a >=> f)

-- Exercise 13.16
twoToThree :: Freer instr a -> Program instr a
twoToThree (Pure' a)        = PDone a
twoToThree (Impure instr k) = PBind (Instr instr) (twoToThree . k)

threeToTwo :: Program instr a -> Freer instr a
threeToTwo (PDone a)               = Pure' a
threeToTwo (Instr instr)           = Impure instr return
threeToTwo (PBind (PDone a) k)     = threeToTwo $ k a
threeToTwo (PBind (Instr instr) k) = Impure instr $ threeToTwo . k
threeToTwo (PBind (PBind x k) k')  =
  threeToTwo $ x `PBind` (k >=> k') -- or threeToTwo   x >>= threeToTwo . (k >=> k')
                                    -- or threeToTwo $ x >>= (k >=> k')

-- | Exercise 13.17
data StackF r
  = Pop (Integer -> r)
  | Push Integer r
  deriving Functor

type Stack = Free StackF

pop :: Stack Integer
pop = liftF $ Pop id

push :: Integer -> Stack ()
push v = liftF $ Push v ()

data RPNInstruction = Number Integer | Plus | Times

eval :: [RPNInstruction] -> Stack Integer
eval []            = pop
eval (Number v: r) = push v >> eval r
eval (Plus: r)     = ((+) <$> pop <*> pop) >>= push >> eval r
eval (Times: r)    = ((*) <$> pop <*> pop) >>= push >> eval r

interpret_ :: StackF a -> State [Integer] a
interpret_ (Pop f)    = do
  values <- get
  case values of
    (a: v) -> put v >> pure (f a)
    []     -> error "empty"
interpret_ (Push v r) = modify (v :) >> pure r

rpnInterpreter :: Stack a -> State [Integer] a
rpnInterpreter = foldFree interpret_

runRPN :: [RPNInstruction] -> Integer
runRPN = ($ []) . evalState . rpnInterpreter . eval

