--module Day19 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Maybe ( fromJust, isNothing )
import Data.Map ( Map, fromList, (!) )

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

data Item = Item {x :: Int,
                  m :: Int,
                  a :: Int,
                  s :: Int}
type Flow = Item -> String

type Interval = (Int, Int)
data IntervalItem = II {x' :: Interval,
                         m' :: Interval,
                         a' :: Interval,
                         s' :: Interval}
    deriving Show
type FlowInterval = IntervalItem -> [(IntervalItem, String)]

type InputType = ([Item], Map String Flow)
type InputType2 = Map String FlowInterval
type ResultType = Int
type ResultType2 = Integer

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?"
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    --let parsedLines = map parseLine1 $ lines
    let parsedLines = parseLines1 lines
    putStrLn $ "Part 1: " ++ show (processInput1 parsedLines)
    --let parsed2Lines = map parseLine2 $ lines
    let parsed2Lines = parseLines2 lines
    putStrLn $ "Part 2: " ++ show (processInput2 parsed2Lines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseItem :: String -> Item
parseItem line = mkItem $ map (read.(drop 2)) $ splitWhen (\c -> c == '{' || c == '}' || c == ',') line
    where mkItem [x,m,a,s] = Item x m a s

parseOp :: String -> Int -> Int -> Bool
parseOp (_:'<':_) = (<)
parseOp (_:'>':_) = (>)

parseFlow :: String -> (String, Flow)
parseFlow line = let (s:ss) = splitWhen (\c -> c == '{' || c == '}' || c == ',') line in (s, mkFlow ss)
    where mkFlow :: [String] -> Item -> String
          mkFlow [c]    _ = c
          mkFlow (c:cs) i = let [v, t, d] = splitWhen (\c -> c == '<' || c == '>' || c == ':') c in if parseOp c (getVal v i) (read t) then d else mkFlow cs i

          getVal :: String -> Item -> Int
          getVal "x" i = x i
          getVal "m" i = m i
          getVal "a" i = a i
          getVal "s" i = s i
                 

parseLines1 :: [String] -> InputType
parseLines1 lines = let [flows, items] = splitWhen (=="") lines in (map parseItem items, fromList $ map parseFlow flows)

parseFlowInterval :: String -> (String, FlowInterval)
parseFlowInterval line = let (s:ss) = splitWhen (\c -> c == '{' || c == '}' || c == ',') line in (s, mkFlowInt ss)
    where mkFlowInt :: [String] -> IntervalItem -> [(IntervalItem, String)]
          mkFlowInt [c]    i = [(i, c)]
          mkFlowInt (c:cs) i = let [v, t, d] = splitWhen (\c -> c == '<' || c == '>' || c == ':') c 
                                   (iPass, iFail) = splitItem (c!!1) v (read t) i 
                                   recu = if isNothing iFail then [] else mkFlowInt cs $ fromJust iFail in 
                                    if isNothing iPass then recu else (fromJust $ iPass, d) : recu
        
          splitItem :: Char -> String -> Int -> IntervalItem -> (Maybe IntervalItem, Maybe IntervalItem)
          splitItem op v t ii = case splitInterval op t (getVal v ii) of
                                (Nothing, Just i) -> (Nothing, Just $ mkIntervalItem v ii i)
                                (Just i, Nothing) -> (Just $ mkIntervalItem v ii i, Nothing)
                                (Just i1, Just i2) -> (Just $ mkIntervalItem v ii i1, Just $ mkIntervalItem v ii i2)

          getVal :: String -> IntervalItem -> Interval
          getVal "x" i = x' i
          getVal "m" i = m' i
          getVal "a" i = a' i
          getVal "s" i = s' i
          
          mkIntervalItem :: String -> IntervalItem -> Interval -> IntervalItem
          mkIntervalItem "x" ii@(II _ m a s) i = II i m a s
          mkIntervalItem "m" ii@(II x _ a s) i = II x i a s
          mkIntervalItem "a" ii@(II x m _ s) i = II x m i s
          mkIntervalItem "s" ii@(II x m a _) i = II x m a i

          splitInterval :: Char -> Int -> Interval -> (Maybe Interval, Maybe Interval)
          splitInterval '<' t i@(l, u) | t <= l = (Nothing, Just i)
                                       | t >= u = (Just i, Nothing)
                                       | otherwise = (Just (l, t-1), Just (t, u))
          splitInterval '>' t i@(l, u) | t <= l = (Nothing, Just i)
                                       | t >= u = (Just i, Nothing)
                                       | otherwise = (Just (t+1, u), Just (l, t))

parseLines2 :: [String] -> InputType2
parseLines2 lines = fromList $ map parseFlowInterval $ head $ splitWhen (=="") lines

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

evalFlow :: Map String Flow -> String -> Item -> Int
evalFlow _     "A" (Item x m a s) = x + m + a + s
evalFlow _     "R" _              = 0
evalFlow flows str i              = evalFlow flows ((flows!str) i) i

processInput1 :: InputType -> ResultType
processInput1 (items, flows) = sum $ map (evalFlow flows "in") items

evalFlowInterval :: Map String FlowInterval -> IntervalItem -> String -> Integer
evalFlowInterval _     (II x m a s) "A" = foldr (\(l, u) t -> toInteger (u - l + 1) * t) 1 [x,m,a,s]
evalFlowInterval _     _            "R" = 0
evalFlowInterval flows ii           str = sum $ map (uncurry $ evalFlowInterval flows) ((flows!str) ii)

processInput2 :: InputType2 -> ResultType2
processInput2 flows = evalFlowInterval flows (II (1, 4000) (1, 4000) (1, 4000) (1, 4000)) "in"