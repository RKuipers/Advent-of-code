--module Day6 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Maybe ( fromJust )
import Data.Char ( isDigit )
import Agda.Syntax.Parser.LexActions ( integer )

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type InputType = ([Int], [Int])
type InputType2 = (Integer, Integer)
type ResultType = Int

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?????"
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    let parsedLines = parseLines1 lines
    putStrLn $ "Part 1: " ++ show (processInput1 parsedLines)
    let parsed2Lines = parseLines2 lines
    putStrLn $ "Part 2: " ++ show (processInput2 parsed2Lines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseLine :: [String] -> [Int]
parseLine line = map read $ tail line

parseLines1 :: [String] -> InputType
parseLines1 [t, d] = (parseLine $ splitWhen (==' ') t, parseLine $ splitWhen (==' ') d)

parseLines2 :: [String] -> InputType2
parseLines2 [t, d] = (integer $ filter isDigit t, integer $ filter isDigit d)

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

processRace :: Int -> Int -> Int -> Int
processRace s 0 d = 0
processRace s t d = curr + processRace (s+1) (t-1) d
   where curr = if s * t > d then 1 else 0

processInput1 :: InputType -> ResultType
processInput1 (ts, ds) = product $ map (\(t, d) -> processRace 0 t d) $ zip ts ds

processRace2 :: Integer -> Integer -> Integer -> Int
processRace2 s 0 d = 0
processRace2 s t d = curr + processRace2 (s+1) (t-1) d
   where curr = if s * t > d then 1 else 0

processInput2 :: InputType2 -> ResultType
processInput2 (t, d) = 2 * processRace2 (t-t1) t1 d
   where t1 = div t 2