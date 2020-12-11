module CH01 where

import           Prelude hiding (pure, (++))

data Tree a = Leaf a | Node (Tree a) (Tree a)

type State s a = s → (a, s)
type WithCounter a = Int → (a, Int)

next ∷ State s a → (a → State s b) → State s b
f `next` g = (\i → let (x, i) = f i in g x i)

then_ ∷ Maybe a → (a → Maybe b) → Maybe b
then_ x f =
  case x of
    Nothing → Nothing
    Just x' → f x'

flatten ∷ Maybe (Maybe a) → Maybe a
flatten oo = then_ oo id

pure ∷ a → WithCounter a
pure x = (\i → (x, i))

pure_ ∷ a → State s a
pure_ x = (\i → (x, i))

relabel ∷ Tree a → WithCounter (Tree (a, Int))
relabel (Leaf x) = (\i → (Leaf (x, i), i + 1))
relabel (Node l r) =
     relabel l `next` (\l' → relabel r `next` (\r' → pure (Node l' r') ))

relabel_ ∷ Tree a → State Int (Tree (a, Int))
relabel_ (Leaf x) = (\i → (Leaf (x, i), i + 1))
relabel_ (Node l r) =
     relabel_ l `next` (\l' → relabel_ r `next` (\r' → pure_ (Node l' r') ))

concat_ ∷ [a] → [a] → [a]
[] `concat_` ys = ys
(x : xs) `concat_` ys = x : (xs `concat_` ys)

