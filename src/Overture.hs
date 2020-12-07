module Overture
    ( takeEvery
    , takeEveryLast
    , withIndex
    , chunksOf
    , replace
    , replaceWhere
    , replicateElems
    , sum'
    , product'
    , (.)
    , (&)
    , ($)
    , (<$>)
    , (<&>)
    , (<<<)
    , (>>>)
    , ($>>)
    , (<<$)
    ) where

import Data.List (foldl')
import Data.Function ((&))
import Data.Functor ((<&>))

(<<<) :: (b -> c) -> (a -> b) -> a -> c
(<<<) = (.)

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

(<<$) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <<$ g = fmap f . g

($>>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f $>> g = fmap g . f

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = replaceWhere (== x) (const y)

replaceWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceWhere _             _         []     = []
replaceWhere shouldReplace transform (x:xs)
  | shouldReplace x = transform x : (replaceWhere shouldReplace transform xs)
  | otherwise       = x : (replaceWhere shouldReplace transform xs)
  

takeEvery :: Int -> [a] -> [a]
takeEvery n []     = []
takeEvery n (x:xs) = x : (takeEvery n (drop (n-1) xs))

takeEveryLast :: Int -> [a] -> [a]
takeEveryLast n list =
  takeEvery n (drop (n-1) list)

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n l =
  let (prefix, suffix) = splitAt n l
  in case suffix of
    [] -> [prefix]
    _  -> prefix : (chunksOf n suffix)

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

product' :: Num a => [a] -> a
product' = foldl' (*) 1

replicateElems :: Int -> [a] -> [a]
replicateElems _ []     = []
replicateElems n (x:xs) = replicate n x ++ (replicateElems n xs)
