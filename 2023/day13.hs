--module Day13 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf, sortBy )
import Data.Maybe ( fromJust, catMaybes )
import Data.Map ( adjust, keys, lookup, Map )
import Data.Function ( on )

import Utils ( splitWhen, parseCoords )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Coord = (Int, Int)
type Pattern = Map Coord Char

type InputType = [Pattern]
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
    putStrLn $ "Part 1: " ++ show (sum $ map (head.processLine1) parsedLines)
    --let parsed2Lines = map parseLine2 $ lines
    --let parsed2Lines = parseLines2 lines
    putStrLn $ "Part 2: " ++ show (sum $ map (head.processLine2) parsedLines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parsePattern :: [String] -> Pattern
parsePattern = parseCoords Just

parseLines1 :: [String] -> InputType
parseLines1 lines = map parsePattern $ splitWhen (=="") lines

parseLines2 :: [String] -> InputType
parseLines2 = parseLines1

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

getColumn :: Pattern -> Int -> Int -> [Char]
getColumn p m x = catMaybes [Data.Map.lookup (x, y) p | y <- [0..m]]

getRow :: Pattern -> Int -> Int -> [Char]
getRow p m y = catMaybes [Data.Map.lookup (x, y) p | x <- [0..m]]

isMirror :: Eq a => [a] -> Bool
isMirror xs = mod (length xs) 2 == 0 && xs == (reverse xs)

checkMirror :: Int -> Int -> [[Char]] -> [Int]
checkMirror x y css | length cs == 0 = []
                    | x > 0 && y > 0 = []
                    | isMirror cs    = [x + div (length cs) 2]
                    | otherwise      = case checkMirror x (y+1) css of
                                       [] -> checkMirror (x+1) y css
                                       xs -> xs ++ checkMirror (x+1) y css
    where cs = reverse $ drop y $ reverse $ drop x css

processLine1 :: Pattern -> [ResultType]
processLine1 p = case checkMirror 0 0 $ map (getRow p maxColumn) [0..maxRow] of
                 [] -> checkMirror 0 0 $ map (getColumn p maxRow) [0..maxColumn]
                 xs  -> map (*100) xs ++ (checkMirror 0 0 $ map (getColumn p maxRow) [0..maxColumn])
    where maxRow = maximum $ map snd $ keys p
          maxColumn = maximum $ map fst $ keys p

processLine2 :: Pattern -> [ResultType]
processLine2 p = go p (head $ processLine1 p) $ keys p
    where go p p1 []     = []
          go p p1 (k:ks) = case processLine1 $ adjust (\c -> if c == '#' then '.' else '#') k p of
                                [] -> go p p1 ks
                                xs -> filter (not.(==p1)) xs ++ go p p1 ks