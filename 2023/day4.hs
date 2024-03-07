--module Day4 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf, all )
import Data.Maybe ( fromJust )
import Data.Char ( isDigit )
import GHC.Utils.Misc ( count )

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type LineType = [Int]
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
    putStrLn $ "Part 1: " ++ show (processInput1 parsedLines)
    let parsed2Lines = map parseLine2 $ lines
    putStrLn $ "Part 2: " ++ show (processInput2 parsedLines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseLine1 :: String -> LineType
parseLine1 line = map read $ filter (\s -> all isDigit s) $ splitWhen (==' ') line

parseLine2 :: String -> LineType
parseLine2 = parseLine1

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

countDuplicates :: Eq a => [a] -> Int
countDuplicates []     = 0
countDuplicates (x:xs) = count (==x) xs + countDuplicates xs

getValue :: Int -> Int
getValue 0 = 0
getValue x = 2 ^ (x-1)

processLine1 :: LineType -> ResultType
processLine1 line = getValue $ countDuplicates line

processInput1 :: [LineType] -> ResultType
processInput1 = foldr (\xs x -> x + processLine1 xs) 0

getMul :: [Int] -> LineType -> [Int]
getMul (m:ms) l = let dups = countDuplicates l in
                  [ms!!(i-1) + if i <= dups then m else 0 | i <- [1..length ms]]

processInput2 :: [LineType] -> ResultType
processInput2 lines = go [1 | _ <- [1..length lines]] lines
   where go _      []     = 0
         go (m:ms) (x:xs) = m + go (getMul (m:ms) x) xs