--module Day16 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf, nub )
import Data.Maybe ( fromJust )
import Data.Map as M ( Map, keys, lookup )

import Utils ( splitWhen, parseCoords )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Coord = (Int, Int)
type Dir = Int

type InputType = Map Coord Char
type ResultType = Int

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?"
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    let parsedLines = parseLines1 lines
    putStrLn $ "Part 1: " ++ show (processInput1 parsedLines 1 (0, 0))
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

parseLines1 :: [String] -> InputType
parseLines1 lines = parseCoords (\c -> if c == '.' then Nothing else Just c) lines

parseLines2 :: [String] -> InputType
parseLines2 = parseLines1

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

step :: Dir -> Coord -> Coord
step 0 (x, y) = (x, y-1)
step 1 (x, y) = (x+1, y)
step 2 (x, y) = (x, y+1)
step 3 (x, y) = (x-1, y)

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

getLength :: Map Coord Char -> (Coord -> Bool) -> [(Dir, Coord)] -> Dir -> Coord -> [(Dir, Coord)]
getLength m oob p d c@(x, y) | oob c  || elem (d, c) p = p
                             | not (elem c $ keys m) = go d
                             | isEven d = let Just s = M.lookup c m in
                                           case s of
                                              '/' -> let d' = (d+1) `mod` 4 in go d'
                                              '\\' -> let d' = (d-1) `mod` 4 in go d'
                                              '-' -> let p' = (go 1) in getLength m oob p' 3 $ step 3 c
                                              '|' -> go d                         
                             | otherwise = let Just s = M.lookup c m in
                                           case s of
                                              '/' -> let d' = (d-1) `mod` 4 in go d'
                                              '\\' -> let d' = (d+1) `mod` 4 in go d'
                                              '-' -> go d
                                              '|' -> let p' = (go 0) in getLength m oob p' 2 $ step 2 c
    where go d' = getLength m oob ((d, c):p) d' $ step d' c

processInput1 :: InputType -> Dir -> Coord -> ResultType
processInput1 m d c = length $ nub $ map snd $ getLength m (\(x, y) -> x < 0 || y < 0 || x > mx || y > my) [] d c
    where mx = maximum $ map fst $ keys m
          my = maximum $ map snd $ keys m

--processLine2 :: LineType -> ResultType
--processLine2 = undefined

processInput2 :: InputType -> ResultType
processInput2 m = maximum $ ([processInput1 m 1 (0, y) | y <- [0..my]] ++ [processInput1 m 2 (x, 0) | x <- [0..mx]] ++ [processInput1 m 3 (mx, y) | y <- [0..my]] ++ [processInput1 m 0 (x, my) | x <- [0..mx]])
    where mx = maximum $ map fst $ keys m
          my = maximum $ map snd $ keys m