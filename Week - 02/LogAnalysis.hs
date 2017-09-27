{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

---------------- Exercise 1 ------------------
parseMessage :: String -> LogMessage
parseMessage txt =
  case words txt of
    ("I" : ts : msg)         -> LogMessage Info (read ts) (unwords msg)
    ("W" : ts : msg)         -> LogMessage Warning (read ts) (unwords msg)
    ("E" : level : ts : msg) -> LogMessage (Error (read level)) (read ts) (unwords msg)
    _                        -> Unknown txt

---------------- Exercise 2 ------------------
parse :: String -> [LogMessage]
parse = map parseMessage . lines

---------------- Exercise 3 ------------------
insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(Unknown _) msgTree     = msgTree
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left msg'@(LogMessage _ ts' _) right)
  | ts < ts'  = Node (insert msg left) msg' right
  | otherwise = Node left msg' (insert msg right)

---------------- Exercise 4 -------------------
build :: [LogMessage] -> MessageTree
build []           = Leaf
build (msg : msgs) = insert msg (build msgs)

---------------- Exercise 5 -------------------
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                     = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right

---------------- Exercise 6 -------------------
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map (\(LogMessage _ _ msg) -> msg)
  . inOrder
  . build
  . filter isSevereError
  where
    isSevereError :: LogMessage -> Bool
    isSevereError (LogMessage (Error level) _ _) = level >= 50
    isSevereError _                              = False
