--module Day14 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf, sortBy )
import Data.Maybe ( fromJust )
import Data.Map as M ( foldrWithKey, lookup, insert, delete, keys, partition, Map ) 
import Data.Function ( on )
import qualified Data.Array as A

import Utils ( splitWhen, parseCoords )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Coord = (Int, Int)

type InputType = ([Coord], [Coord])
type ResultType = Int

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?"
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    let parsedLines = parseLines lines
    putStrLn $ "Part 1: " ++ show (processInput1 parsedLines)
    putStrLn $ "Part 2: " ++ show (processInput2 parsedLines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseSymbol :: Char -> Maybe Int
parseSymbol '.' = Nothing
parseSymbol '#' = Just 0
parseSymbol 'O' = Just 1

parseLines :: [String] -> InputType
parseLines lines = (keys bs, keys rs)
    where parsed = parseCoords parseSymbol lines
          (bs, rs) = partition (==1) parsed 

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

tiltNorth :: [Coord] -> [Coord] -> [Coord]
tiltNorth rs bs' = go [] $ sortBy (compare `on` snd) bs'
    where go bs' []          = bs'
          go bs' ((x, y):bs) = go ((x, 1+blocking x y (rs ++ bs' ++ bs)):bs') bs
          blocking x y xs = maximum $ -1:(map snd $ filter (\(x', y') -> x == x' && y' < y) xs)

processInput1 :: InputType -> ResultType
processInput1 (bs, rs) = foldr (\(x, y) t -> t + maxY - y + 1) 0 $ tiltNorth rs bs
    where maxY = maximum $ map snd (bs ++ rs)

tiltWest :: [Coord] -> [Coord] -> [Coord]
tiltWest rs bs' = go [] $ sortBy (compare `on` fst) bs'
    where go bs' []          = bs'
          go bs' ((x, y):bs) = go ((1+blocking x y (rs ++ bs' ++ bs), y):bs') bs
          blocking x y xs = maximum $ -1:(map fst $ filter (\(x', y') -> x' < x && y' == y) xs)

tiltSouth :: Int -> [Coord] -> [Coord] -> [Coord]
tiltSouth my rs bs' = go [] $ sortBy (flip compare `on` snd) bs'
    where go bs' []          = bs'
          go bs' ((x, y):bs) = go ((x, blocking x y (rs ++ bs' ++ bs) - 1):bs') bs
          blocking x y xs = minimum $ (my+1):(map snd $ filter (\(x', y') -> x == x' && y' > y) xs)

tiltEast :: Int -> [Coord] -> [Coord] -> [Coord]
tiltEast mx rs bs' = go [] $ sortBy (flip compare `on` fst) bs'
    where go bs' []          = bs'
          go bs' ((x, y):bs) = go ((blocking x y (rs ++ bs' ++ bs) - 1, y):bs') bs
          blocking x y xs = minimum $ (mx+1):(map fst $ filter (\(x', y') -> x' > x && y' == y) xs)

processInput2 :: InputType -> ResultType
processInput2 (bs', rs) = go 1000000000 [bs']
    where maxY = maximum $ map snd (bs' ++ rs)
          maxX = maximum $ map fst (bs' ++ rs)
          applyCycle = tiltEast maxX rs.tiltSouth maxY rs.tiltWest rs.tiltNorth rs
          getLoad = foldr (\(x, y) t -> t + maxY - y + 1) 0
          go 0   bss@(bs:_) = getLoad bs
          go rem bss@(bs:_) = let bs' = applyCycle bs in
                                     if not (elem bs' bss) then go (rem - 1) (bs':bss)
                                     else go ((rem `mod` (length bss - fromJust (elemIndex bs' $ reverse bss)))-1) (bs':bss)