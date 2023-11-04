{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Text.Read (readMaybe)

import Log

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
    ("E":severity:timestamp:rest) ->
        case (readMaybe severity, readMaybe timestamp) of
            (Just sev, Just ts) -> LogMessage (Error sev) ts (unwords rest)
            _ -> Unknown msg
    ("W":timestamp:rest) ->
        case readMaybe timestamp of
            Just ts -> LogMessage Warning ts (unwords rest)
            _ -> Unknown msg
    ("I":timestamp:rest) ->
        case readMaybe timestamp of
            Just ts -> LogMessage Info ts (unwords rest)
            _ -> Unknown msg
    _ -> Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines