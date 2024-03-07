--module Day5 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Maybe ( fromJust )
import Data.Char ( isDigit )

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Interval = (Int, Int)
type Mapping1 = Int -> Int
type Mapping2 = Interval -> [Interval]

type InputType1 = ([Int], [Mapping1])
type InputType2 = ([Interval], [Mapping2])
type ResultType = Int

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?"
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    let parsedLines@(seeds, mappings) = parseLines1 lines
    putStrLn $ "Part 1: " ++ show (processInput1 parsedLines)
    let parsedLines2@(seeds2, mappings2) = parseLines2 lines
    putStrLn $ "Part 2: " ++ show (processInput2 parsedLines2)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseMap1 :: [Int] -> Int -> Int
parseMap1 [a, b, c] x = if x >= b && x < b + c then x + (a-b)
                       else -1
parseMap1 _         x = -1

parseMapping1 :: [String] -> Mapping1
parseMapping1 []     n = n
parseMapping1 (x:xs) n = let r = parseMap1 $ map read $ splitWhen (==' ') x in
                         if r n >= 0 then r n
                         else parseMapping1 xs n

parseLines1 :: [String] -> InputType1
parseLines1 (x:xs) = (seeds x, map parseMapping1 $ splitWhen (=="") xs)
    where seeds line = map read $ splitWhen (==' ') $ drop 7 line

splitInterval :: Interval -> Int -> (Interval, Interval)
splitInterval (s, e) splitPoint = if s < splitPoint then if e >= splitPoint then ((s, splitPoint), (splitPoint, e)) else ((s, e), (splitPoint, splitPoint)) else ((splitPoint, splitPoint), (s, e))

parseMap2 :: [[Int]] -> Interval -> [Interval]
parseMap2 []             i = [i]
parseMap2 ([]:ms)        i = parseMap2 ms i
parseMap2 ([d, s, l]:ms) i = if fst p2 == snd p2 then concatMap (parseMap2 ms) $ filter (\i -> not $ fst i == snd i) [p1, p2, p3] else p2:(concatMap (parseMap2 ms) $ filter (\i -> not $ fst i == snd i) [p1, p3])
    where (p1, p1') = splitInterval i s
          (p2', p3) = splitInterval p1' (s+l)
          p2 = (fst p2' - s + d , snd p2' - s + d)

parseLine :: String -> [Int]
parseLine []       = []
parseLine xs@(x:_) | isDigit x = map read $ splitWhen (==' ') xs
                   | otherwise = []

parseMapping2 :: [String] -> Mapping2
parseMapping2 xs = parseMap2 $ map parseLine xs

parseLines2 :: [String] -> InputType2
parseLines2 (x:xs) = (seeds x, map parseMapping2 $ splitWhen (=="") xs)
    where seeds line = mkSeedIntervals $ map read $ splitWhen (==' ') $ drop 7 line
          mkSeedIntervals [] = []
          mkSeedIntervals (start:length:xs) = (start, start+length):mkSeedIntervals xs

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

compose :: [a -> a] -> a -> a
compose = flip (foldl (flip id))

processInput1 :: InputType1 -> ResultType
processInput1 (seeds, mappings) = minimum $ map (compose mappings) seeds

processInput2 :: InputType2 -> ResultType
processInput2 (seedIntervals, [])     = minimum $ map fst seedIntervals
processInput2 (seedIntervals, (m:ms)) = processInput2 (concatMap m seedIntervals, ms)