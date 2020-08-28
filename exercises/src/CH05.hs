module CH05 where


-- | Excercise 5.1
newtype RMaybe a = RMaybe (Maybe a) deriving Show
newtype LMaybe a = LMaybe (Maybe a) deriving Show

instance Semigroup (RMaybe a) where
  RMaybe Nothing <> right = right
  left           <>  _    = left

instance Monoid (RMaybe a) where
  mempty  = RMaybe Nothing

instance Semigroup (LMaybe a) where
  LMaybe Nothing <> right = right
  _              <> right = right

instance Monoid (LMaybe a) where
  mempty  = LMaybe Nothing

