module Util where

sequenceTup2 :: (Applicative m) => (m a, m b) -> m (a, b)
sequenceTup2 (x, y) = (,) <$> x <*> y
