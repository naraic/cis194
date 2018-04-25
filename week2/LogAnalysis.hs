{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where 

import Log

parseMessage :: String -> LogMessage
parseMessage m = parseWords (words m)

parseWords :: [String] -> LogMessage
parseWords ("I":(t:m))             = LogMessage Info (read t) (unwords m)
parseWords ("W":(t:m))             = LogMessage Warning (read t) (unwords m)
parseWords ("E":(e:(t:m)))         = LogMessage (Error (read e)) (read t) (unwords m)
parseWords m                       = Unknown (unwords m)

parse :: String -> [LogMessage]
parse m    = parseMessage <$> lines m

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node left n@(LogMessage _ time _) right) 
       | t < time = Node (insert m left) n right  
       | t > time = Node left n (insert m right)

build :: [LogMessage] -> MessageTree
build logs = buildHelper logs Leaf

buildHelper :: [LogMessage] -> MessageTree -> MessageTree
buildHelper [] t = t
buildHelper (m:ms) t = buildHelper ms (insert m t)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ (m : inOrder right)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = prioritise (inOrder (build msgs))

prioritise :: [LogMessage] -> [String]
prioritise [] = []
prioritise ((LogMessage (Error l) _ mesg) : ms)
    | l >= 50 = (mesg : prioritise ms)
    | otherwise = prioritise ms
prioritise (_:ms) = prioritise ms



    


