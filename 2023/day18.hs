--module Day18 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf, splitAt )
import Data.Maybe ( fromJust )
import Data.Char ( isAlpha, isNumber, isAlphaNum, digitToInt )

import Utils ( splitWhen, shoeLace )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Dir = Int
type Coord = (Int, Int)

type LineType = (Dir, Int)
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
    let parsed2Lines = map parseLine2 $ lines
    --let parsed2Lines = parseLines2 lines
    putStrLn $ "Part 2: " ++ show (processInput2 parsed2Lines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseDir :: String -> Dir
parseDir "U" = 0
parseDir "R" = 1
parseDir "D" = 2
parseDir "L" = 3

parseLine1 :: String -> LineType
parseLine1 line = go $ splitWhen (==' ') line
    where go (dir:dis:[colour]) = (parseDir dir, read dis)      

parseHex :: String -> (Dir, Int)
parseHex s = let (dis, dir) = splitAt 5 s in (((digitToInt $ head dir) + 1) `mod` 4, go $ reverse dis)
    where go []        = 0
          go (x:xs)    = digitToInt x + 16 * go xs

parseLine2 :: String -> LineType
parseLine2 line = parseHex $ filter isAlphaNum $ head $ drop 2 $ splitWhen (==' ') line

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

move :: Coord -> Dir -> Int -> Coord
move c dir 0   = c
move c dir dis = move next dir (dis - 1)
    where next = step dir c
          step :: Dir -> Coord -> Coord
          step 0 (x, y) = (x, y-1)
          step 1 (x, y) = (x+1, y)
          step 2 (x, y) = (x, y+1)
          step 3 (x, y) = (x-1, y)

digTrench :: Coord -> [(Dir, Int)] -> [Coord]
digTrench c []              = []
digTrench c ((dir, dis):xs) = end: digTrench end xs
    where end = move c dir dis

distance :: Coord -> Coord -> Int
distance (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

boundaryLength :: Coord -> [Coord] -> Int
boundaryLength _  []       = 0          
boundaryLength x1 [x]      = distance x x1
boundaryLength x1 (x:y:xs) = distance x y + boundaryLength x1 (y:xs)

processInput1 :: InputType -> ResultType
processInput1 commands = let path = digTrench (0, 0) commands
                             boundary = boundaryLength (head path) path in
                             (shoeLace path) - (div boundary 2) + 1 + boundary

--processLine2 :: LineType -> ResultType
--processLine2 = undefined

processInput2 :: InputType -> ResultType
processInput2 = processInput1