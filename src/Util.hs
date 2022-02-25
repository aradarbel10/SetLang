module Util where

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

orElse :: Maybe a -> a -> a
orElse Nothing a = a
orElse (Just a) _ = a

takeEnd :: Integer -> [a] -> [a]
takeEnd n = reverse . take (fromInteger n) . reverse

(!) :: a -> b -> (a, b)
(!) x y = (x, y)