module CH07 where

import Control.Applicative
import Prelude hiding (Either(..))
import Control.Monad.Logic hiding (guard)

-- | Exercise 7.1

data Either e r = Left e | Right r deriving Show

instance Functor (Either e) where
  fmap f (Right r) = Right $ f r
  fmap _ (Left e) = Left e

instance Applicative (Either e) where
  pure x = Right x
  Left e <*> _ = Left e
  _ <*> Left e = Left e
  Right f <*> Right x = Right $ f x

instance Monad (Either e) where
  Left e >>= _ = Left e
  Right x >>= f = f x

-- | Exercise 7.2

instance Monoid e => Alternative (Either e) where
  empty = Left mempty
  Right a <|> _ = Right a
  _ <|> Right a = Right a
  Left e1 <|> Left e2 = Left $ e1 <> e2

-- | Exercise 7.3

type Person = String

guard :: Alternative m => Bool -> m ()
guard True  = pure ()
guard False = empty

people :: [Person]
people = ["Alejandro", "Elena", "Quique", "John", "Mary", "Tom"]

pcRels :: [(Person, Person)]
pcRels = [ ("Alejandro", "Quique")
         , ("Elena", "Quique")
         , ("John", "Mary")
         , ("John", "Tom")
         , ("Mary", "Tim")
         ]

gpgcRels :: [(Person, Person)]
gpgcRels = do
  (grandparent, parent) <- pcRels
  (parent', child')     <- pcRels
  guard (parent' == parent)
  return (grandparent, child')

siblingRels :: [(Person, Person)]
siblingRels = do
  (parent, child)   <- pcRels
  (parent', child') <- pcRels
  guard (parent == parent' && child /= child')
  return (child', child)

-- | Exercise 7.4

pyts :: [Integer] -> Logic (Integer, Integer, Integer)
pyts = undefined

