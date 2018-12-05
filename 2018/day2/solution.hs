{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List

main :: IO ()
main = runSolution solution1

solution2 :: [String] -> String
solution2 list = "TODO"

solution1 :: [String] -> Int
solution1 = reduce . map f
  where
    f :: String -> (Int, Int)
    f str =
      let uniqNums = counts str
       in (existsToInt 2 uniqNums, existsToInt 3 uniqNums)

-- Helpers
runSolution :: (Show a) => ([String] -> a) -> IO ()
runSolution solution = print . solution . lines =<< readFile "input.txt"

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
