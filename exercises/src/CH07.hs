module CH07 where

import           Control.Applicative
import           Control.Monad.Logic hiding (guard)
import           Prelude             hiding (Either (..))

-- | Exercise 7.1

data Either e r = Left e | Right r deriving Show

instance Functor (Either e) where
  fmap f (Right r) = Right $ f r
  fmap _ (Left e)  = Left e

instance Applicative (Either e) where
  pure x = Right x
  Left e  <*> _       = Left e
  _       <*> Left e  = Left e
  Right f <*> Right x = Right $ f x

instance Monad (Either e) where
  Left e  >>= _ = Left e
  Right x >>= f = f x

-- | Exercise 7.2

instance Monoid e => Alternative (Either e) where
  empty = Left mempty
  Right a <|> _       = Right a
  _       <|> Right a = Right a
  Left e1 <|> Left e2 = Left $ e1 <> e2

-- | Exercise 7.3

type Person = String

guard :: Alternative m => Bool -> m ()
guard True  = pure ()
guard False = empty

people :: [Person]
people = [ "Alejandro"
         , "Elena"
         , "Quique"
         , "John"
         , "Mary"
         , "Tom"
         ]

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

fairTriples :: [Integer] -> Logic (Integer, Integer, Integer)
fairTriples ns =
  list ns >>- \x ->
  list ns >>- \y ->
  list ns >>- \z ->
  return (x, y, z)

list :: [a] -> Logic a
list xs = msum (map return xs)

sums :: [Integer] -> Logic (Integer, Integer, Integer)
sums ns = fairTriples ns >>=
  \(x, y, z) -> do
    guard (x + y == z)
    return (x, y, z)

pyts :: [Integer] -> Logic (Integer, Integer, Integer)
pyts ns = fairTriples ns >>=
  \(x, y, z) -> do
    guard (x ^ 2 + y ^ 2 == z ^ 2)
    return (x, y, z)

-- | Exercise 7.5

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
  throwError = Left
  catchError (Left e) f = f e
  catchError m _        = m

instance MonadError () Maybe where
  throwError e = Nothing
  catchError Nothing f = f ()
  catchError m _       = m

