{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}

module Experiments where

import           Prelude                hiding (take)

import           CH13                   (Board (..), Player (..), Position (..),
                                         RPNInstruction (..), TicTacToe,
                                         Tr (..), emptyBoard, info, runRPN,
                                         take)
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
  Ap   :: f c -> Ap f (c -> b) -> Ap f b

instance Functor (Ap f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Ap a b) = Ap a $ (fmap (f .) b)

instance Functor f => Applicative (Ap f) where
  pure = Pure
--  Pure f <*> a = fmap f a
--  Ap a b <*> c = Ap a $ (flip <$> b <*> c)
  f <*> Pure b = fmap ($ b) f
  f <*> Ap a b = Ap a ((.) <$> f <*> b)

data Arg a
  = Flag String   (Bool -> a)
  | Option String (Maybe String -> a)
  deriving Functor

type CommandLine = Ap Arg

flag :: String -> CommandLine Bool
flag a = Ap (Flag a id) (Pure id)

option :: String -> CommandLine (Maybe String)
option a = Ap (Option a id) (Pure id)

data Config = Config { background :: Bool
                     , file       :: String
                     }

readCommandLine :: CommandLine Config
readCommandLine =
  Config <$> flag "background" <*> (maybe "out" id <$> option "file")

argNames :: CommandLine a -> [String]
argNames (Pure _) = []
argNames (arg `Ap` rest) = (name arg) : argNames rest
  where name :: Arg a -> String
        name (Flag   v _) = "flag " <> v
        name (Option v _) = "option " <> v

