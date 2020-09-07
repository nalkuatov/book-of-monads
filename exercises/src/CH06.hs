module CH06 where

-- | Exercise 6.1
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State x) = State $ \s -> let (a, s') = x s in (f a, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  State f <*> State x = State $ \s ->
    let
     (f', s')  = f s
     (a,  s'') = x s'
    in
      (f' a, s'')

