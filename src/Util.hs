module Util where
import Distribution.Simple.Utils (xargs)

import qualified Data.Set as S

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (x, y) = (f x, y)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

orElse :: Maybe a -> a -> a
orElse Nothing a = a
orElse (Just a) _ = a

justOr :: Maybe a -> Maybe a -> Maybe a
justOr (Just x) _ = Just x
justOr Nothing x = x 

fromJust :: Maybe a -> a
fromJust Nothing = error "nothing"
fromJust (Just a) = a

takeEnd :: Integer -> [a] -> [a]
takeEnd n = reverse . take (fromInteger n) . reverse

(!) :: a -> b -> (a, b)
(!) x y = (x, y)

firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe _ [] = Nothing
firstMaybe f (x:xs) = f x `justOr` firstMaybe f xs

xor :: Bool -> Bool -> Bool
xor x y = x /= y

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

getLeft :: Either a b -> a
getLeft (Left x) = x
getLeft (Right _) = error "unexpected"

-- given a list of lists,
-- return all possible ways to choose one element from each list
chooseOnes :: [[a]] -> [[a]]
chooseOnes = foldl (\acc lst -> concatMap (\choice -> map (\x -> choice ++ [x]) lst) acc) [[]]

untilNothing :: (a -> Maybe a) -> a -> a
untilNothing f a = case f a of
    Nothing -> a
    Just a' -> untilNothing f a'

unpair :: [(a, a)] -> [a]
unpair = uncurry (++) . unzip

unpairSet :: Ord a => S.Set (a, a) -> S.Set a
unpairSet = S.unions . S.map (\(x, y) -> S.fromList [x, y])

boolMaybe :: a -> Bool -> Maybe a
boolMaybe a False = Nothing
boolMaybe a True  = Just a