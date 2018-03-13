
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
