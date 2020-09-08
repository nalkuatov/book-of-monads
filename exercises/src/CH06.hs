module CH06 where

-- | Exercise 6.1
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State x) = State $ \s -> let (a, s') = x s in (f a, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  State f <*> State x = State $ \s ->
    let (f', s')  = f s
        (a,  s'') = x s'
    in  (f' a, s'')

instance Monad (State s) where
  State x >>= f = State $ \s ->
    let (x', s') = x s
    in runState (f x') s'

-- | Exercise 6.2
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put $ f s

