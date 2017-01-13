module Advent1 where

import Prelude hiding(Right, Left)
import qualified Data.Set as Set

data Turn = Right Int | Left Int deriving (Eq, Show)
--east positive/west negative, south positive, north negative
newtype Position = Position { unPosition :: (Int,Int) } deriving (Eq, Show, Ord)
data Direction = North | East | South | West deriving (Eq, Show, Ord)

solution1 = (total . snd . last . produceAllStepsForTurns) turns

solution2 = total  <$> ((findFirstDuplicate . map snd . produceAllStepsForTurns) turns)


findFirstDuplicate :: Ord a => [a] -> Maybe a
findFirstDuplicate xs = go xs Set.empty
    where go [] _            = Nothing
          go (x:xs) s
            | Set.member x s = Just x
            | otherwise      = go xs (Set.insert x s)


produceAllStepsForTurns :: [Turn] -> [(Direction, Position)]
produceAllStepsForTurns = concat . scanl (\history turn -> doSingleMove (last history) turn) [(North, Position (0,0))]

doSingleMove :: (Direction, Position) -> Turn -> [(Direction, Position)]
doSingleMove (dir, p) t = 
    let newDir = turn dir t
        moveSpaces = getMoveSpaces t
        newPositions = move p moveSpaces newDir
    in  (,) newDir <$> newPositions

move :: Position -> Int -> Direction -> [Position]
move (Position (x,y)) n North = [Position (x, y - i) | i <- [1 .. n]]
move (Position (x,y)) n South = [Position (x, y + i) | i <- [1 .. n]]
move (Position (x,y)) n East  = [Position (x + i, y) | i <- [1 .. n]]
move (Position (x,y)) n West  = [Position (x - i, y) | i <- [1 .. n]]

getMoveSpaces :: Turn -> Int
getMoveSpaces (Right n) = n
getMoveSpaces (Left n) = n

turn :: Direction -> Turn -> Direction
turn North (Right _) = East
turn North (Left _) = West
turn South (Right _) = West
turn South (Left _) = East
turn East (Right _) = South
turn East (Left _) = North
turn West (Right _) = North
turn West (Left _) = South

stringToTurn :: String -> Turn
stringToTurn [] = error "Invalid input to stringToTurn"
stringToTurn s@(hd:tail) 
              | hd == 'R' = Right (tailToInt tail)
              | hd == 'L' = Left (tailToInt tail)
              | otherwise = error $ "Invalid input" ++ s
              where tailToInt = read . (takeWhile (/= ','))


input = "L1, L5, R1, R3, L4, L5, R5, R1, L2, L2, L3, R4, L2, R3, R1, L2, R5, R3, L4, R4, L3, R3, R3, L2, R1, L3, R2, L1, R4, L2, R4, L4, R5, L3, R1, R1, L1, L3, L2, R1, R3, R2, L1, R4, L4, R2, L189, L4, R5, R3, L1, R47, R4, R1, R3, L3, L3, L2, R70, L1, R4, R185, R5, L4, L5, R4, L1, L4, R5, L3, R2, R3, L5, L3, R5, L1, R5, L4, R1, R2, L2, L5, L2, R4, L3, R5, R1, L5, L4, L3, R4, L3, L4, L1, L5, L5, R5, L5, L2, L1, L2, L4, L1, L2, R3, R1, R1, L2, L5, R2, L3, L5, L4, L2, L1, L2, R3, L1, L4, R3, R3, L2, R5, L1, L3, L3, L3, L5, R5, R1, R2, L3, L2, R4, R1, R1, R3, R4, R3, L3, R3, L5, R2, L2, R4, R5, L4, L3, L1, L5, L1, R1, R2, L1, R3, R4, R5, R2, R3, L2, L1, L5"

turns = (fmap stringToTurn . words) input

total (Position (x, y)) = abs x + abs y
