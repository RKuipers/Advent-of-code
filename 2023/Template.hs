--module Day where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Maybe ( fromJust )

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type LineType = Int
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
    putStrLn $ "Part 1: " ++ show (processInput1 parsedLines)
    --let parsed2Lines = map parseLine2 $ lines
    --let parsed2Lines = parseLines2 lines
    --putStrLn $ "Part 2: " ++ show (processInput2 parsed2Lines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseLines1 :: [String] -> InputType
parseLines1 lines = undefined

parseLine1 :: String -> LineType
parseLine1 line = undefined

parseLines2 :: [String] -> InputType
parseLines2 = parseLines1

parseLine2 :: String -> LineType
parseLine2 = parseLine1

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

--processLine1 :: LineType -> ResultType
--processLine1 = undefined

processInput1 :: InputType -> ResultType
processInput1 = undefined

--processLine2 :: LineType -> ResultType
--processLine2 = undefined

processInput2 :: InputType -> ResultType
processInput2 = processInput1