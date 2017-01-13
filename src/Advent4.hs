{-# LANGUAGE RecordWildCards #-}

module Advent4 where  

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Ord as O
import qualified Data.Foldable as F
import Data.Monoid
import Data.Char

data CharFreq = CharFreq Char Int deriving (Show, Eq)

getCharFrom (CharFreq c _) = c

instance Ord (CharFreq) where 
  (CharFreq c1 i1) <= (CharFreq c2 i2) 
      | (i1 < i2) = True
      | (i1 == i2 && c1 > c2) = True
      | otherwise = False


data Room = Room { frequencies :: [CharFreq],
                   sector      :: Int,
                   checksum    :: String,
                   rawName     :: String } deriving (Show, Eq)

--Make room from the room name (with - removed) and the sectornumber/hash
mkRoom :: (String, String) -> Room
mkRoom (chars, sectorAndHash) = Room (createFreqFromMap (mkMap chars)) sector hash chars
                  where mkMap s           =  M.fromListWith (+) [(x, 1) | x <- s]
                        createFreqFromMap = fmap (uncurry CharFreq) . M.toList
                        sector            = read $ takeWhile isNumber sectorAndHash
                        hash              = (dropRight 1 . drop 1 . dropWhile isNumber) sectorAndHash


getSectorMaybe :: Room -> Maybe Int
getSectorMaybe Room {..} = 
  let sortedLetters = fmap getCharFrom $ L.sortBy (O.comparing O.Down) frequencies
  in case (take 5 sortedLetters) == checksum of True -> Just sector
                                                False -> Nothing


dropRight n = reverse . drop n . reverse

parseLine :: String -> Room
parseLine = mkRoom . span (not . isNumber) . filter (/= '-')

shiftRoom :: Room -> (String, Room)
shiftRoom r @ (Room _ by _ chars) = 
  let wrap i  = (i - ord 'a') `mod` 26 + (ord 'a')
      shift c = chr $ wrap $ ord c + by
  in (fmap shift chars, r)

solution :: [Room] -> Int 
solution rooms =  foldr (+) 0 (concat (fmap (F.toList . getSectorMaybe) rooms))

printSolutions :: IO ()
printSolutions = do
    content <- readFile "input/advent4Input.txt"
    let linesOfFiles = lines content
    let rooms = fmap parseLine linesOfFiles
    print (show (solution rooms))
    let storageRoomSector = fmap (sector . snd) $ L.find (\tuple -> L.isInfixOf "north" $ fst tuple) $ fmap shiftRoom rooms
    print storageRoomSector
