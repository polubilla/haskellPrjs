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

-- parse: Parses a whole log file
parse :: String -> [LogMessage]
parse "" = []
parse s = map parseMessage logLines
  where 
    logLines = lines s
-------
-- EOF:
-------

{- Test Results:

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
