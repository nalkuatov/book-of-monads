module Experiments where


failList ∷ [Int]
failList = do
  (x : _) ← []
  return x

