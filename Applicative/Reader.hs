newtype Reader e a = Reader { apply :: e -> a }

instance Functor (Reader e) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader e) where
  pure x = Reader (const x)
  -- (<*>) (Reader ff) (Reader g) = Reader (\e -> ff e (g e))
  liftA2 f (Reader ff) (Reader fg) = Reader (\e -> f (ff e) (fg e))

