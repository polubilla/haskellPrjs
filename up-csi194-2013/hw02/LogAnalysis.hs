{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module LogAnalysis where

import Log

-- Exercise 1
--------------
-- parseMessage : Parses an individual line from the log file
-- examples:
-- error  : parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- info   : parseMessage "I 29 la la la"     == LogMessage Info 29 "la la la"
-- warning: parseMessage "W 41 Achtung"      == LogMessage Warning 41 "Achtung"
-- unknown: parseMessage "This is not in the right format" == LogMessage Unknown "This is not in the right format"
--
-- Para el pattern matching se considera el siguiente formato del mensaje x:y:z, 
-- x: tipo de mensaje: I, E , W
-- y: errorLevel o TimeStampo
-- z: descripcion del Mensaje
--------------
parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  "I":y:z   -> LogMessage Info  (read y) (unwords z)             ---(read y :: TimeStamp)
  "E":x:y:z -> LogMessage (Error (read x)) (read y) (unwords z)  
  "W":y:z   -> LogMessage Warning (read y) (unwords z)  
  _         -> Unknown "This is not in the right format"

{- anterior
parseMessage s = case msgType of
  "E" -> LogMessage (Error errorLvl) etimeStmp elogMsg
  "I" -> LogMessage Info timeStmp logMsg
  "W" -> LogMessage Warning timeStmp logMsg
  _   -> Unknown "This is not in the right format"
  where
    msg       = words s
    msgType   = head msg  
    errorLvl  = read (unwords (drop 1 msg))
    etimeStmp = read (unwords (drop 2 msg))
    elogMsg   = unwords (drop 3 msg)
    timeStmp  = read (unwords (drop 1 msg))
    logMsg    = unwords (drop 2 msg)
-}

-- parse: Parses a whole log file
parse :: String -> [LogMessage]
parse "" = []
parse s = map parseMessage logLines
  where 
    logLines = lines s


-- Exercise 2
-- LogMessage getters -----------------------------------
messageType :: LogMessage -> MessageType
messageType (LogMessage mType _ _) = mType
messageType (Unknown _) = undefined

timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage _ mTimeStamp _) = mTimeStamp
timeStamp (Unknown _) = 0

logMessage :: LogMessage -> String
logMessage (LogMessage _ _ msg) = msg
logMessage (Unknown _) = ""

validLogMsg :: LogMessage -> Bool
validLogMsg LogMessage{} = True   --(LogMessage _ _ _) = True
validLogMsg (Unknown _) = False

severity :: MessageType -> Int
severity (Error s) = s
severity _         = 0

---------------------------------------------------------

-- Returns True if the timeStamp of the first logmsg is greater than the second
greaterTimeStamp :: LogMessage -> LogMessage -> Bool
greaterTimeStamp m n = tsm > tsn
  where
  tsm = timeStamp m
  tsn = timeStamp n
 
-- Insert in a sorted messageTree a new logMessage
insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg (Node left nodeMsg right) =
  if greaterTimeStamp logMsg nodeMsg && validLogMsg logMsg
  then insert logMsg right
  else insert logMsg left 
    

-- Exercise 3
-- Build a complete MessageTree from a list of messages
build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = Node Leaf x (build xs)

-- Exercise 4
-- Take a sorted MessageTree and produce a list of all the LogMessages it contains, 
-- sorted by timestamp from smallest to biggest
-- inOrder (build tree)
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree logMsg rTree) = inOrder lTree ++ [logMsg] ++ inOrder rTree

-- Exercise 5
-- Extract the relevant information. 
-- Relevant means "errors with a severtiy of at least 50"
sortLogMessages :: [LogMessage] -> [LogMessage]
sortLogMessages = inOrder.build
--sortLogMessages [] = inOrder (build [])
--sortLogMessages lm = inOrder (build lm)

-- Takes a sorted list of LogMessages and returns a list of the messages
-- with severity > 50
severityMaj50 ::[LogMessage] -> [String]
severityMaj50 [] = []
severityMaj50 ((LogMessage mType _ msg) : xs) = 
  if severity mType>50     -- Convendria implementar una funcion isError :: mType -> Bool
  then msg : severityMaj50 xs
  else severityMaj50 xs
severityMaj50 (Unknown _ : xs) = severityMaj50 xs
 
-- Takes an unsorted list of LogMessages and returns a list of the messages
-- corresponding to any errors with a severst  of 50 or greater
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = severityMaj50.sortLogMessages

-------
-- EOF:
-------

{- Test Results

ghci> testWhatWentWrong parse whatWentWrong "error.log"
[
"Twenty seconds remaining until out-of-mustard condition",
"Hard drive failure: insufficient mustard",
"Empty mustard reservoir! Attempting to recover...",
"Depletion of mustard stores detected!",
"Recovery failed! Initiating shutdown sequence",
"All backup mustardwatches are busy",
"Mustardwatch opened, please close for proper functioning!",
"Ten seconds remaining until out-of-mustard condition"
]

-}