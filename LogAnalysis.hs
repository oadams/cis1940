{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg
  | msgType == "E" = LogMessage (Error first) (read (words msg !! 2) :: Int) (unwords $ drop 3 $ words msg) 
  | msgType == "W" = LogMessage Warning first rest
  | msgType == "I" = LogMessage Info first rest
  | otherwise = Unknown msg
  where
    msgType = head $ words msg
    first = read (words msg !! 1) :: Int
    rest = unwords $ drop 2 $ words msg


parse :: String -> [LogMessage]
parse = map parseMessage . lines

--main = do
--  print $ parseMessage "a b c"


-- Questions:
-- 1. How do we handle the possibility of line not starting with I, W, or E without taining our code with maybes?
-- 2. How do we get the Error constructor to apply to the first to words while the others apply to only 1?
-- The first question really cuts at the heart of something I want to know about how Haskell interacts with real world data.
-- Should have read the assignment properly, that's what the Unknown constructor is for.

