
import Log

parseMessage :: String -> LogMessage
parseMessage m = parseWords (words m)

parseWords :: [String] -> LogMessage
parseWords ("I":(t:m))             = LogMessage Info (read t) (unwords m)
parseWords ("W":(t:m))             = LogMessage Warning (read t) (unwords m)
parseWords ("E":(e:(t:m)))         = LogMessage (Error (read e)) (read t) (unwords m)
parseWords m                       = Unknown (unwords m)

parse :: String -> [LogMessage]
parse []    = []
parse (m:ms)  = (parseMessage m):(parse ms)

