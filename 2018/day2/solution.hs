{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List

main :: IO ()
main = print . reduce . map f . lines =<< readFile "input.txt"
  where
    f :: String -> (Int, Int)
    f str =
      let uniqNums = counts str
       in (existsToInt 2 uniqNums, existsToInt 3 uniqNums)

sumTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTuples (a, b) (c, d) = (a + c, b + d)

reduce :: [(Int, Int)] -> Int
reduce = uncurry (*) . foldl sumTuples (0, 0)

existsToInt :: Int -> [Int] -> Int
existsToInt n list =
  if n `elem` list
    then 1
    else 0

counts :: String -> [Int]
counts str = map (`count` str) (nub str)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
