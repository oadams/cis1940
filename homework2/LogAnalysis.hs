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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) tree@(Node left msg2@(LogMessage _ ts2 _) right)
    | ts < ts2 = Node (insert msg left) msg2 right
    | ts >= ts2 = Node left msg2 (insert msg right)
    | otherwise = tree -- This made the compiler stop complaining about inexhaustive patterns
insert _ tree@(Node _ (Unknown _) _) = tree -- This shouldn't happen

build' :: [LogMessage] -> MessageTree -> MessageTree
build' [] tree = tree
build' (m:ms) tree = build' ms (insert m tree)

build :: [LogMessage] -> MessageTree
build ms = build' ms Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

{-
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error sev) _ s):msgs)
    | sev >= 50 = [s] ++ (whatWentWrong msgs)
    | otherwise = whatWentWrong msgs
whatWentWrong (_:msgs) = whatWentWrong msgs
-}

wantMessage :: LogMessage -> Bool
wantMessage (LogMessage (Error sev) _ _) = sev >= 50
wantMessage _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = [s | (LogMessage _ _ s) <- filtered]
    where ordered = inOrder $ build msgs
          filtered = [msg | msg <- ordered, wantMessage msg]


main :: IO ()
main = do
    msgs <- take 10 . parse <$> readFile "error.log" 
    print msgs
    print $ inOrder $ build msgs