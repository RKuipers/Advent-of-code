module Day12 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Maybe ( fromJust )
import qualified Data.Array as A

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type LineType = (String, [Int])
type InputType = [LineType]
type ResultType = Int

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?"
    --file <- getLine
    lines <- readLinesFromFile ("inputs/day_12_large.txt")
    let parsedLines = map parseLine1 $ lines
    --let parsedLines = parseLines1 lines
    putStrLn $ "Part 1: " ++ show (sum $ map processLine1 parsedLines)
    let parsed2Lines = map parseLine2 $ lines
    --let parsed2Lines = parseLines2 lines
    putStrLn $ "Part 2: " ++ show (sum $ map processLine2 parsed2Lines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseLine1 :: String -> LineType
parseLine1 line = let split = splitWhen (==' ') line in
                  (head split, map read $ splitWhen (==',') $ last split)

rep :: Int -> [a] -> [a] -> [a]
rep 1 xs sep = xs
rep r xs sep = xs ++ sep ++ rep (r-1) xs sep

parseLine2 :: String -> LineType
parseLine2 line = let (cs, xs) = parseLine1 line in
                  (rep 5 cs "?", rep 5 xs [])

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

safeHead :: a -> [a] -> a
safeHead d []    = d
safeHead _ (x:_) = x

processLine1 :: LineType -> ResultType
processLine1 (cs        , [])         = if all (not.(=='#')) cs then 1 else 0
processLine1 ([]        , _)          = 0
processLine1 (css@(c:cs), xss@(x:xs)) | sum xss + length xss - 1 > length css = 0
                                      | otherwise                             = case c of 
                                                                                '?' -> processLine1 (cs, xss) + processLine1 ('#':cs, xss)
                                                                                '.' -> processLine1 (cs, xss)
                                                                                '#' -> if all (not.(=='.')) $ take x css then if length css >= x && (not.(=='#').(safeHead '.') $ drop x css) then processLine1 (drop x cs, xs) else 0 else 0

processLine2 :: LineType -> ResultType
processLine2 (css, xss) = go css xss
    where lc = length css
          lx = length xss
          memory = A.array ((0, 0), (length css, length xss)) [((x, y), pl (drop x css, drop y xss)) | x <- [0..lc], y <- [0..lx]]
          go css' xss' = memory A.! (lc - length css', lx - length xss')
          pl (cs        , [])         = if all (not.(=='#')) cs then 1 else 0
          pl ([]        , _)          = 0
          pl (css@(c:cs), xss@(x:xs)) | sum xss + length xss - 1 > length css = 0
                                      | otherwise                             = case c of 
                                                                                '?' -> go cs xss + pl (('#':cs), xss)
                                                                                '.' -> go cs xss
                                                                                '#' -> if all (not.(=='.')) $ take x css then if length css >= x && (not.(=='#').(safeHead '.') $ drop x css) then go (drop x cs) xs else 0 else 0