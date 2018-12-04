{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe (fromMaybe)
import qualified Data.Set   as S (empty, insert, member)

main :: IO ()
main = runSolution part1 >> runSolution part2

part1 :: (Num a) => [a] -> a
part1 = sum

part2 :: [Int] -> Int
part2 = fromMaybe 0 . findRepeat . scanl (+) 0 . cycle

-- Helpers
runSolution :: (Show a) => ([Int] -> a) -> IO ()
runSolution f = print . f . map parseLine . lines =<< readFile "input.txt"

findRepeat :: Ord a => [a] -> Maybe a
findRepeat = check S.empty
  where
    check existing (x:xs)
      | x `S.member` existing = Just x
      | otherwise = check (x `S.insert` existing) xs
    check _ [] = Nothing

parseLine :: String -> Int
parseLine [] = 0
parseLine (x:xs) =
  case x of
    '-' -> negate (read xs :: Int)
    '+' -> read xs :: Int
    _   -> 0
