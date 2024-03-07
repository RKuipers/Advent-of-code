module Day10 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Maybe ( fromJust )
import Data.Map ( empty, insert, lookup, keys, union, fromList, Map )

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Coord = (Int, Int)

type InputType = (Coord, Coord, Map Coord (Coord -> Coord))
type ResultType = Int

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?"
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    --let parsedLines = map parseLine1 $ lines
    let parsedLines = parseLines1 lines
    putStrLn $ "Part 1: " ++ show (processInput1 parsedLines)
    --let parsed2Lines = map parseLine2 $ lines
    --let parsed2Lines = parseLines2 lines
    putStrLn $ "Part 2: " ++ show (processInput2 parsedLines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

nextCoord :: Char -> Coord -> Coord -> Coord
nextCoord '|' self@(x1, y1) start@(x2, y2) = if y1 > y2 then (x1, y1+1) else (x1, y1-1)
nextCoord '-' self@(x1, y1) start@(x2, y2) = if x1 > x2 then (x1+1, y1) else (x1-1, y1)
nextCoord 'L' self@(x1, y1) start@(x2, y2) = if y1 == y2 then (x1, y1-1) else (x1+1, y1)
nextCoord 'J' self@(x1, y1) start@(x2, y2) = if y1 == y2 then (x1, y1-1) else (x1-1, y1)
nextCoord '7' self@(x1, y1) start@(x2, y2) = if y1 == y2 then (x1, y1+1) else (x1-1, y1)
nextCoord 'F' self@(x1, y1) start@(x2, y2) = if y1 == y2 then (x1, y1+1) else (x1+1, y1)

parseSymbol :: Int -> Int -> String -> [(Coord, Coord -> Coord)]
parseSymbol x y []     = []
parseSymbol x y (c:cs) | c == '.' || c == 'S' = parseSymbol (x+1) y cs
                       | otherwise            = ((x, y), nextCoord c (x, y)) : parseSymbol (x+1) y cs

parseSymbols :: Int -> [String] -> Map Coord (Coord -> Coord)
parseSymbols y []     = empty
parseSymbols y (l:ls) = union (fromList $ parseSymbol 0 y l) (parseSymbols (y+1) ls)

parseLines1 :: [String] -> InputType
parseLines1 lines = let (x, y) = findS 0 lines in ((x, y), (x, y+1), parseSymbols 0 lines)
    where findS y (l:ls) = case elemIndex 'S' l of
                             Nothing -> findS (y+1) ls
                             Just x  -> (x, y)

parseLines2 :: [String] -> InputType
parseLines2 line = undefined

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

traversePipes :: Map Coord (Coord -> Coord) -> Coord -> Coord -> Coord -> Int
traversePipes m start curr next | start == next = 1
                                | otherwise     = 1 + (traversePipes m start next $ (fromJust $ Data.Map.lookup next m) curr)

processInput1 :: InputType -> ResultType
processInput1 (s, n, m) = traversePipes m s s n `div` 2

getLoop :: Map Coord (Coord -> Coord) -> Coord -> Coord -> Coord -> [Coord]
getLoop m start curr next | start == next = [next]
                          | otherwise     = next : (getLoop m start next $ (fromJust $ Data.Map.lookup next m) curr)

shoeLace :: [Coord] -> Int
shoeLace [_]                        = 0
shoeLace ((x1, y1) : (x2, y2) : xs) = (y1 + y2) * (x2 - x1) + shoeLace ((x2, y2) : xs)

processInput2 :: InputType -> ResultType
processInput2 (s, n, m) = let path = getLoop m s s n in (abs (shoeLace path) - length path + 3) `div` 2