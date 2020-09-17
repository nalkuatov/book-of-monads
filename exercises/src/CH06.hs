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

-- | Experiments on `Reader r a`
newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader x) = Reader $ \r -> f $ x r

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x
  Reader f <*> Reader x =
    Reader $ \r -> let a = x r in f r a

instance Monad (Reader r) where
  Reader a >>= f =
    Reader $ \r ->
      let mb = f $ a r in runReader mb r

newtype Writer w a = Writer { runWriter :: (w, a) }

