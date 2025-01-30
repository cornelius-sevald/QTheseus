{-# LANGUAGE OverloadedStrings #-}

module Util where

import Prettyprinter (Pretty (pretty))

sequenceTup2 :: (Applicative m) => (m a, m b) -> m (a, b)
sequenceTup2 (x, y) = (,) <$> x <*> y

lookup' :: (Eq k, Pretty k, Pretty v) => k -> [(k, v)] -> v
lookup' k xs = case lookup k xs of
  Just v -> v
  Nothing -> error $ show $ "Could not find key '" <> pretty k <> "' in " <> pretty xs

headMay :: [a] -> Maybe a
headMay (x : _) = Just x
headMay [] = Nothing

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Right x) = Right (f x)
mapRight _ (Left x) = Left x

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

boolToInt :: (Num n) => Bool -> n
boolToInt False = 0
boolToInt True = 1
