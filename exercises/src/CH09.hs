{-# LANGUAGE RankNTypes #-}

module CH09 where

-- Exercise 9.1

newtype Cont r a =
  Cont { unCont :: (a -> r) -> r
       }

instance Functor (Cont r) where
  fmap f (Cont arr) =
    Cont $ \br ->
      arr $ br . f

instance Applicative (Cont r) where
  pure a = Cont $ \f -> f a
  Cont abrr <*> arr =
    Cont $ \br -> abrr (\ab -> (unCont $ fmap ab arr) br)

instance Monad (Cont r) where
  Cont arr >>= f =
    Cont $ \br -> arr (\a -> unCont (f a) br)

toCont :: a -> (forall r. Cont r a)
toCont a = Cont $ \ar -> ar a

fromCont :: (forall r. Cont r a) -> a
fromCont (Cont f) = f id

