module Util where
import Distribution.Simple.Utils (xargs)

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (x, y) = (f x, y)

orElse :: Maybe a -> a -> a
orElse Nothing a = a
orElse (Just a) _ = a

justOr :: Maybe a -> Maybe a -> Maybe a
justOr (Just x) _ = Just x
justOr Nothing x = x 

takeEnd :: Integer -> [a] -> [a]
takeEnd n = reverse . take (fromInteger n) . reverse

(!) :: a -> b -> (a, b)
(!) x y = (x, y)

firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe _ [] = Nothing
firstMaybe f (x:xs) = f x `justOr` firstMaybe f xs

-- given a list of lists,
-- return all possible ways to choose one element from each list
chooseOnes :: [[a]] -> [[a]]
chooseOnes = foldl (\acc lst -> concatMap (\choice -> map (\x -> choice ++ [x]) lst) acc) [[]]