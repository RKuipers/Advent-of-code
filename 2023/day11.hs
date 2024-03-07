--module Day11 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf, subsequences )
import Data.Maybe ( fromJust )
import Data.Map ( keys )

import Utils ( splitWhen, parseCoords )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Coord = (Int, Int)
type InputType = [Coord]
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
    putStrLn $ "Coords: " ++ show (length parsedLines)
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

parseChar :: Char -> Maybe Int
parseChar '.' = Nothing
parseChar '#' = Just 0

parseLines1 :: [String] -> InputType
parseLines1 lines = keys $ parseCoords parseChar lines

parseLines2 :: [String] -> InputType
parseLines2 = parseLines1

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

expand :: Int -> Int -> Int -> [Int] -> [Int]
expand incr m x cs | x >= m    = cs
                   | elem x cs = expand incr m (x+1) cs
                   | otherwise = expand incr (m+incr) (x+incr+1) $ map (\y -> if y > x then y+incr else y) cs

getPairs :: [a] -> [(a, a)]
getPairs []     = []
getPairs (x:xs) = map (\y -> (x, y)) xs ++ getPairs xs

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getDistances :: [Coord] -> [Int]
getDistances xs = map (uncurry manhattanDistance) $ getPairs xs

processInput :: Int -> InputType -> ResultType
processInput incr cs = sum $ getDistances $ zip (expand incr (maximum xs) 0 xs) (expand incr (maximum ys) 0 ys)
   where xs = map fst cs
         ys = map snd cs

processInput1 :: InputType -> ResultType
processInput1 = processInput 1

processInput2 :: InputType -> ResultType
processInput2 = processInput 999999