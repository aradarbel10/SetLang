module Util where

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

orElse :: Maybe a -> a -> a
orElse Nothing a = a
orElse (Just a) _ = a