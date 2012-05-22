module Utils( fromMaybe
            , foldl', sortWith, groupWith
            , (?), updateVec
            , foldM, liftM, join, guard, joinMaybes, select, (<$>)
            , zipmap, mapKeys, reverseMap
            , (|.|)
            , frequencies, repeatedElems
            , infinity, cartesianProduct, combinationsOf, combinations
            , indexedList
            ) where

import qualified Data.Map as Map
import qualified Data.Vector as Vec
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad
import Data.List (foldl', group, sort, find)
import Data.Tuple (swap)
import Data.Functor ((<$>))
import GHC.Exts (sortWith, groupWith)

-- Better error reporting
infixr 1 ?
(?) :: Maybe a -> String -> a
x ? s = fromMaybe (error s) x

updateVec :: Vec.Vector a -> [(Int, a)] -> String -> Vec.Vector a
updateVec vec vals fnName = if all (< len) indices
                              then Vec.unsafeUpd vec vals -- is Vec.force needed here?
                              else error $ fnName ++ ": index/indices out of bounds (" ++ show (filter (>= len) indices) ++ ", " ++ show len ++ ")"
  where len = Vec.length vec
        indices = map fst vals


-- Monadic stuff
joinMaybes :: MonadPlus m => m (Maybe a) -> m a
joinMaybes = (>>= maybe mzero return)

-- To understand this function let's instantiate it for the list monad:
-- select :: [a] -> [(a,[a])]
-- select [] = []
-- select (a:as) = [(a,as)] ++ liftM (map (a:)) (select as)  =  (a,as) : map (fmap (a:)) (select as)  =  (a,as) : map (\(b,bs) -> (b,a:bs)) (select as)
select :: MonadPlus m => [a] -> m (a,[a])
select [] = mzero
select (a:as) = return (a,as) `mplus` liftM (fmap (a :)) (select as)


-- Maps
zipmap :: (Ord a) => [a] -> [b] -> Map.Map a b
zipmap as bs = Map.fromList $ zip as bs

mapKeys :: (Ord a) => (a -> b) -> [a] -> Map.Map a b
mapKeys f ks = Map.fromList . zip ks $ map f ks

reverseMap :: (Ord a, Ord b) => Map.Map a b -> Map.Map b a
reverseMap = Map.fromList . map swap . Map.toList


-- Functions
pcomp :: (a -> b) -> (a -> c) -> a -> (b, c)
pcomp f g a = (f a, g a)

infixl 8 |.|
f |.| g = pcomp f g


-- Counting
frequencies :: (Ord a) => [a] -> [(a, Int)]
frequencies = map (head |.| length) . group . sort

repeatedElems :: (Ord a) => [a] -> [a]
repeatedElems = map fst . filter ((>1) . snd) . frequencies


-- Math
infinity :: (Fractional a) => a
infinity = 1/0

-- Combinatorics
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs:xss) = [ x:ys | x <- xs, ys <- cartesianProduct xss ]

combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

combinations :: Int -> Int -> [[Int]]
combinations k n = combinationsOf k [0..n-1]


-- Vectors

indexedList :: Vec.Vector a -> [(Int, a)]
indexedList = Vec.toList . Vec.indexed

