import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf, stripPrefix, uncons)
import Data.Maybe ( fromJust )

import Utils ( splitWhen )

data Game = Game { id :: Int,
                  red :: Int,
                 blue :: Int, 
                green :: Int}
    deriving (Show)
    
type LineType = Game
type ResultType = Int

main :: IO ()
main = do
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


parseLine1 :: String -> LineType
parseLine1 line = parseWords 0 0 0 0 0 $ splitWhen (\c -> c==' ' || c==':' || c==',' || c==';') line

parseLine2 :: String -> LineType
parseLine2 = parseLine1

parseWords :: Int -> Int -> Int -> Int -> Int -> [String] -> Game
parseWords id red blue green recent words = case uncons words of
                                   Nothing             -> Game id red blue green
                                   Just ("Game", x:xs) -> parseWords (read x) red blue green recent xs
                                   Just ("red", xs)    -> parseWords id (max recent red) blue green recent xs
                                   Just ("blue", xs)   -> parseWords id red (max recent blue) green recent xs
                                   Just ("green", xs)  -> parseWords id red blue (max recent green) recent xs
                                   Just (x, xs)        -> parseWords id red blue green (read x) xs

processInput1 :: [LineType] -> ResultType
processInput1 = foldr (\g x -> x + fitsBag g) 0

fitsBag :: Game -> Int
fitsBag (Game id red blue green) = if red <= 12 && blue <= 14 && green <= 13 then id
                                   else 0

processInput2 :: [LineType] -> ResultType
processInput2 = foldr (\(Game id red blue green) x -> x + red * blue * green) 0