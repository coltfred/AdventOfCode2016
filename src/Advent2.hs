module Advent2 where

import Data.Maybe

data Dir = U | D | L | R deriving (Eq, Show)
--(X,Y) - Right is positive, Down is Positive
newtype Loc = Loc { unLoc :: (Int,Int) } deriving (Eq, Show)

--drop 1 because the seed value isn't part of the solution.
solution1 :: [[Dir]] -> [String]
solution1 =  solution loc2Digit (Loc (1,1))

--drop 1 because the seed value isn't part of the solution.
solution2 :: [[Dir]] -> [String]
solution2 =  solution loc2Digit2 (Loc (0,2))

solution :: (Loc -> Maybe String) -> Loc -> [[Dir]] -> [String]
solution loc2MaybeDigit start = concatMap (maybeToList . loc2MaybeDigit) . drop 1 . scanl (getLineSolution loc2MaybeDigit) start

loc2Digit :: Loc -> Maybe String
loc2Digit (Loc t) = 
  case t of
   (0,0) -> Just "1"
   (1,0) -> Just "2"
   (2,0) -> Just "3"
   (0,1) -> Just "4"
   (1,1) -> Just "5"
   (2,1) -> Just "6"
   (0,2) -> Just "7"
   (1,2) -> Just "8"
   (2,2) -> Just "9"
   _     -> Nothing


loc2Digit2 :: Loc -> Maybe String
loc2Digit2 (Loc t) = 
  case t of
   (2,0) -> Just "1"
   (1,1) -> Just "2"
   (2,1) -> Just "3"
   (3,1) -> Just "4"
   (0,2) -> Just "5"
   (1,2) -> Just "6"
   (2,2) -> Just "7"
   (3,2) -> Just "8"
   (4,2) -> Just "9"
   (1,3) -> Just "A"
   (2,3) -> Just "B"
   (3,3) -> Just "C"
   (2,4) -> Just "D"
   _ -> Nothing

parseLine :: String -> [Dir]
parseLine = fmap charToDir

charToDir 'U' = U
charToDir 'D' = D
charToDir 'L' = L
charToDir 'R' = R
charToDir c = error $ "Invalid input" ++ [c]


getLineSolution :: (Loc -> Maybe a) -> Loc -> [Dir] -> Loc
getLineSolution locCheck start = foldl (move (isJust . locCheck)) start 

move :: (Loc -> Bool) -> Loc -> Dir -> Loc
move locCheck l dir = 
  let newLoc     = naiveMove l dir
  in case locCheck newLoc of
      True  -> newLoc
      False -> l

naiveMove :: Loc -> Dir -> Loc
naiveMove (Loc (x, y)) U   = Loc(x, y - 1)
naiveMove (Loc (x, y)) D   = Loc(x, y + 1)
naiveMove (Loc (x, y)) L   = Loc(x - 1, y)
naiveMove (Loc (x, y)) R   = Loc(x + 1, y)

printSolutions :: IO ()
printSolutions = do
    content <- readFile "input/advent2Input.txt"
    let linesOfFiles = lines content
    let dirs = fmap parseLine linesOfFiles
    print $ "Solution: " ++ show (solution1 dirs)
    print $ "Solution2: " ++ show (solution2 dirs)
