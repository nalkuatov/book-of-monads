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

newtype Writer w a =
  Writer { runWriter :: (w, a) }

instance Functor (Writer w) where
  fmap f (Writer (w, a)) = Writer (w, f a)

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (mempty, x)
  Writer (w1, f) <*> Writer (w2, a) =
    Writer (w1 <> w2, f a)

instance Monoid w => Monad (Writer w) where
  Writer (w1, a) >>= f =
    let Writer (w2, b) = f a
    in Writer (w1 <> w2, b)

-- | Exercise 6.3

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

newtype Returns r a = R (a -> r)

instance Contravariant (Returns r) where
  contramap f (R g) = R $ g . f

