--module Day where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Maybe ( fromJust )

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type LineType = [Int]
type InputType = [LineType]
type ResultType = Int

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?"
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    let parsedLines = map parseLine1 $ lines
    --let parsedLines = parseLines1 lines
    putStrLn $ "Part 1: " ++ show (sum $ map processLine1 parsedLines)
    --let parsed2Lines = map parseLine2 $ lines
    --let parsed2Lines = parseLines2 lines
    putStrLn $ "Part 2: " ++ show (sum $ map processLine2 parsedLines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

--parseLines1 :: [String] -> InputType
--parseLines1 line = undefined

parseLine1 :: String -> LineType
parseLine1 line = map read $ splitWhen (==' ') line

parseLines2 :: [String] -> InputType
parseLines2 line = undefined

parseLine2 :: String -> LineType
parseLine2 = parseLine1

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

getNextLine :: [Int] -> [Int]
getNextLine [x] = []
getNextLine (x:y:xs) = y - x:getNextLine (y:xs)

processLine1 :: LineType -> ResultType
processLine1 [] = 0
processLine1 xs = last xs + processLine1 (getNextLine xs)

processLine2 :: LineType -> ResultType
processLine2 [] = 0
processLine2 xs = head xs - processLine2 (getNextLine xs)