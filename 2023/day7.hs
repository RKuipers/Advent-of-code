--module Day7 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf, group, sort )
import Data.Maybe ( fromJust )
import Data.Char ( digitToInt )
import Data.List.Extra ( allSame )

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Card = Int
data HandType = High | One | Two | Three | Full | Four | Five
    deriving (Show, Eq, Ord)
data Hand = Hand { cards :: [Card],
                   bid :: Int}
    deriving (Show, Eq)

mostCommon :: Ord a => [a] -> (Int, a)
mostCommon = maximum . map (\xs -> (length xs, head xs)) . group . sort

handType :: Hand -> HandType
handType (Hand c _) | elem 1 c  = maximum $ map (\cs -> handType $ Hand cs 0) $ map (\x -> map (\y -> if y == 1 then x else y) c) [2..14]
                    | otherwise = case mostCommon c of
                        (5,_)     -> Five
                        (4,_)     -> Four
                        (3,v)     -> if allSame $ filter (/=v) c then Full else Three
                        (2,v)     -> if (fst $ mostCommon $ filter (/=v) c) == 2 then Two else One
                        (1,_)     -> High

instance Ord Hand where
    (<=) h1@(Hand c1 _) h2@(Hand c2 _) | handType h1 == handType h2 = if c1 == c2 then True else (\x -> fst x <= snd x) $ head $ filter (\x -> fst x /= snd x) $ zip c1 c2
                                       | otherwise                  = handType h1 <= handType h2

type LineType = Hand
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

parseCard :: Char -> Card
parseCard 'A' = 14
parseCard 'K' = 13
parseCard 'Q' = 12
parseCard 'J' = 11
parseCard 'T' = 10
parseCard c = digitToInt c

parseLine1 :: String -> LineType
parseLine1 line = let [cs, b] = splitWhen (==' ') line in
                  Hand (map parseCard cs) (read b)

parseCard2 :: Char -> Card
parseCard2 'J' = 1
parseCard2 c = parseCard c

parseLine2 :: String -> LineType
parseLine2 line = let [cs, b] = splitWhen (==' ') line in
                  Hand (map parseCard2 cs) (read b)

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

processInput1 :: InputType -> ResultType
processInput1 hs = sum $ zipWith (*) [1..] $ map (\(Hand _ b) -> b) $ sort hs

--processLine2 :: LineType -> ResultType
--processLine2 = undefined

processInput2 :: InputType -> ResultType
processInput2 = processInput1