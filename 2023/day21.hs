--module Day21 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf, findIndex )
import Data.Maybe ( fromJust )
import Data.Map as M ( Map, fromList, (!), keys, union, filter )
import Data.Sequence as S ( Seq(..), singleton, (><), fromList )

import Utils ( splitWhen, parseCoords, isEven )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Coord = (Int, Int)
type InputType = (Map Coord Int, Coord)
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

findCoord :: (a -> Bool) -> [[a]] -> Coord
findCoord p xs = (fromJust $ findIndex p (xs!!y), y)
    where y = fromJust $ findIndex (any p) xs

parseLines1 :: [String] -> InputType
parseLines1 lines = (parseCoords (\c -> if c == '#' then Just (-1) else Nothing) lines, findCoord (=='S') lines)

parseLines2 :: [String] -> InputType
parseLines2 = parseLines1

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

neighbors :: Map Coord a -> Coord -> [Coord]
neighbors m (x, y) = Prelude.filter (\c@(x', y') -> x' >= 0 && y' >= 0 && x' <= mx && y' <= my && (not $ elem c km)) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    where km = keys m
          mx = maximum $ map fst km
          my = maximum $ map snd km

bfs :: Seq (Int, Coord) -> Map Coord Int -> Map Coord Int
bfs Empty          m = m
bfs ((0, c) :<| q) m = m
bfs ((d, c) :<| q) m = bfs q' m'
    where neighs = neighbors m c
          m' = union m $ M.fromList [(n, d-1) | n <- neighs]
          q' = q >< (S.fromList [(d-1, n) | n <- neighs])

processInput1 :: InputType -> ResultType
processInput1 (m, s) = length $ keys $ M.filter isEven $ bfs (singleton (64, s)) m

processInput2 :: InputType -> ResultType
processInput2 = processInput1