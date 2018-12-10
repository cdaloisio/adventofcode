{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Data.Char     (isDigit)
import           Data.Foldable (toList)
import           Data.Ix       (Ix, inRange, index, range)
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe    (mapMaybe)
import           Text.Read     (readMaybe)

main :: IO ()
main = runSolution solution1

solution1 :: [String] -> Maybe Int
solution1 = findOverlapping . getTiles
  where
    getTiles :: [String] -> Maybe [Rect]
    getTiles = traverse (fmap claimRect . parseClaim)
    findOverlapping :: Maybe [Rect] -> Maybe Int
    findOverlapping Nothing = Nothing
    findOverlapping (Just tiles') =
      Just $ length $ filter (>= 2) $ M.elems $ layTiles tiles'

tiles :: Rect -> [Coord]
tiles (Rect start size) = range (start, start + size - 1)

layTiles :: [Rect] -> Map Coord Int
layTiles = frequencyMap . concatMap tiles
  where
    frequencyMap = M.fromListWith (+) . map (, 1) . toList

-- Data types
data Claim = Claim
  { claimId   :: Int
  , claimRect :: Rect
  }

data Coord = Coord
  { coordX :: Int
  , coordY :: Int
  } deriving (Eq, Ord, Show)

instance Ix Coord where
  range (Coord l1 l2, Coord u1 u2) =
    [Coord i1 i2 | i1 <- range (l1, u1), i2 <- range (l2, u2)]
  index = undefined
  inRange = undefined

instance Num Coord where
  (+) (Coord a b) (Coord c d) = Coord (a + c) (b + d)
  (*) (Coord a b) (Coord c d) = Coord (a * c) (b * d)
  abs = undefined
  signum = undefined
  fromInteger int = Coord (fromIntegral int) (fromIntegral int)
  negate (Coord a b) = Coord (negate a) (negate b)

data Rect = Rect
  { rectStart :: Coord
  , rectSize  :: Coord
  } deriving (Show)

makeClaim :: [Int] -> Maybe Claim
makeClaim [i, x, y, w, h] = Just $ Claim i (Rect (Coord x y) (Coord w h))
makeClaim _               = Nothing

-- Parser
parseClaim :: String -> Maybe Claim
parseClaim = makeClaim . mapMaybe readMaybe . words . remove (not . isDigit)
  where
    remove p =
      map $ \x ->
        if p x
          then ' '
          else x

-- Helpers
runSolution :: (Show a) => ([String] -> a) -> IO ()
runSolution solution = print . solution . lines =<< readFile "input.txt"
