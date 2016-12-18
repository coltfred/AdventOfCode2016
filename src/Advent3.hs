module Advent3 where

import Data.List

sortTuple (x, y, z) = (x',y',z')
          where [x', y', z'] = sort [x,y,z]

legalTri :: [Integer] -> Bool
legalTri [a,b,c] = a + b > c 
legalTri _ = False


solution1 :: [[Integer]] -> Int
solution1 = length .filter legalTri . fmap sort 

solution2 :: [[Integer]] -> Int
solution2 triList = solution1 $ (transpose triList) >>= window 3

--Produce windows of length i in the list, stepping by i 
window _ [] = []
window i as       
        | length window' == i = window' : window i (drop i as)
        | otherwise = []
        where window' = take i as

parseLine :: String -> [Integer]
parseLine = fmap read . words

printSolutions :: IO ()
printSolutions = do
    content <- readFile "input/advent3Input.txt"
    let linesOfFiles = lines content
    let ints = fmap parseLine linesOfFiles
    print $ "Total legal triangles: " ++ show (solution1 ints)
    print $ "Total legal triangles vertically: " ++ show (solution2 ints)
    
