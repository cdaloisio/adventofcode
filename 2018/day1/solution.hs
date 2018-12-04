{-# LANGUAGE OverloadedStrings #-}

module Main where

main :: IO ()
main = readFile "input.txt" >>= print . sum . map parseLine . lines

parseLine :: String -> Int
parseLine [] = 0
parseLine (x:xs) =
  case x of
    '-' -> negate (read xs :: Int)
    '+' -> read xs :: Int
    _   -> 0
