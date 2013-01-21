module Main where

import Data.List ((\\))
--import Data.Maybe 
--import Control.Monad
import Control.Monad.State
import Control.Applicative 
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Console.Readline

--import Bag
import Syntax
import Parser
import Printer
import RewriteRules
import Reductions
import ProverState
import Util
import Term



changeEnvTo :: [Term] -> ProverState -> ProverState
changeEnvTo newEnv state = state { env = linearizeTensorProducts newEnv } 

addToEnv :: [Term] -> ProverState -> ProverState
addToEnv resources state = state {env = (env state)++(linearizeTensorProducts resources)} 

-- TODO: removeFromEnv is quadratic!; can this improve?
removeFromEnv :: [Term] -> ProverState -> ProverState
removeFromEnv resources state = state {env = (env state) \\ (linearizeTensorProducts resources) }

changeDebugMode :: ProverState -> ProverState
changeDebugMode state = state {debugMode = not (debugMode state)} 

changeGranularityTo :: Int -> ProverState -> ProverState
changeGranularityTo n state = state { granularity = n }


-- ask user for string
-- TODO: Improve reliability; tabs/auto-complete (readline), etc.
readStringFromUser :: String -> IO String
readStringFromUser query = do
  flushStr query
  answer <- readline ""
  case answer of
    Nothing -> return ""
    Just line -> return line

toggleDebugMode :: ProverStateIO ProverState
toggleDebugMode = do
    state <- get
    let inDebugMode = debugMode state
    if inDebugMode then lift (flushStrLn "Debug mode turned off.")
                   else lift (flushStrLn "Debug mode turned on.")
    return (changeDebugMode state)

changeGranularity :: ProverStateIO ProverState
changeGranularity = do
    state <- get
    let currentGranularity = granularity state
    g <- lift $ askUserForInt (>0) ("Enter new value for granularity (current value is " ++ (show currentGranularity) ++ "): ")
    return (changeGranularityTo g state)


askUserForInt :: (Int->Bool) -> String -> IO Int
askUserForInt validate query = do
    flushStr query
    answer <- getLine
    let n = reads answer :: [(Int, String)]
    case n of
        []      -> flushStrLn "Not a number. Please try again!" >> askUserForInt validate query
        (i,_):_ -> if (validate i) then return i
                                   else flushStrLn "Invalid number. Please try again!" >> askUserForInt validate query
    


removeTrailingSpace :: String -> String
removeTrailingSpace s | last s == ' ' = init s
                      | otherwise     = s

loadFile :: ProverStateIO ProverState
loadFile = do
    f <- lift $ readStringFromUser "Load file: "
    let fileName = removeTrailingSpace f
    state <- get
    fileExists <- lift $ doesFileExist fileName
    if (fileExists) then do fileContents <- lift $ readFile fileName 
                            state <- resetAllCounters
                            return (changeEnvTo (tts' fileContents) state)
                    else lift (flushStrLn $ "ERROR: File '" ++ fileName ++ "' does not exist!") >> return state

removeResource :: ProverStateIO ProverState
removeResource = do
    resources <- lift $ readStringFromUser "Enter resources to remove: "
    state <- get
    case parse term "<interactive>" resources of
     Left err -> do
                  lift $ flushStrLn "ERROR: Parsing error. Please try again." 
                  return state
     Right r -> return (removeFromEnv (tts' resources) state)

addResource :: ProverStateIO ProverState
addResource = do
    resources <- lift $ readStringFromUser "Enter resources to add: "
    state <- get
    case parse term "<interactive>" resources of
     Left err -> do
                  lift $ flushStrLn "ERROR: Parsing error. Please try again." 
                  return state
     Right r -> return (addToEnv (tts' resources) state)

startReductions :: ProverStateIO ProverState
startReductions = do
    state <- get
    let inDebugMode = debugMode state
    when inDebugMode $
        do lift $ print $ "[DEBUG] ALL ACTIONS: " ++ show ((env state)) 
    -- TODO: ASK FOR ENABLED ACTIONS
    --newEnv <- reduceStateIO' (map simplify (env state)) 
    --reduceStateIO' (env state) 
    startTeLLer
    lift $ flushStrLn "End of story, no more reductions found."
    state <- get
    
    treductions <- getNumberTotalReductions
    lift $ flushStrLn $ (show treductions) ++ " reductions."
    newState <- get
    return newState 
    
printEnv state = do flushStrLn $ "\nCurrent (focused) resources: \n" ++ showTerms (env state) ++ "\n" 
                    flushStrLn $ "\nCurrent (unfocused) resources: \n" ++ showTerms (unfocused state) ++ "\n" 

-- the main loop for interacting with the user
mainLoop :: ProverState -> IO ProverState
mainLoop state = do
  let inDebugMode = debugMode state
  when inDebugMode $ printEnv state 
  comm <- readline "Command [d+-glpsqr?]: "
  flushStr "\n"
  case comm of
    Nothing -> flushStrLn "Goodbye." >> return state
    Just c  -> 
     do addHistory c 
        continue <- case c of
            ('d':_) -> do newEnv <- evalStateT toggleDebugMode state; return (Just newEnv)
            ('+':_) -> do newEnv <- evalStateT addResource state; return (Just newEnv)
            ('-':_) -> do newEnv <- evalStateT removeResource state; return (Just newEnv)
            ('g':_) -> do newEnv <- evalStateT changeGranularity state; return (Just newEnv)
            ('l':_) -> do newEnv <- evalStateT loadFile state; return (Just newEnv)
            ('p':_) -> if inDebugMode then return (Just state) else printEnv state >> return (Just state)
            ('q':_) -> return Nothing
            ('r':_) -> return (Just initialState)
            ('s':_) -> do newEnv <- evalStateT startReductions state; return (Just newEnv) 
            ('?':_) -> flushStrLn helpOptions >> return (Just state)
            ('\n':_) -> return (Just state)
            _       -> do flushStrLn $ "Command " ++ c ++ " not recognized."
                          return (Just state)
        maybe (return state) mainLoop continue
  

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> do fileExists <- doesFileExist f
              if(fileExists) then printWelcomeMessage >> createEnvFromFile f >>= mainLoop
                             else printWelcomeMessage >> flushStrLn "ERROR: File passed in command line does not exist." >> mainLoop initialState
              return ()
    []  -> do printWelcomeMessage
              env <- mainLoop initialState
              return ()

printWelcomeMessage = flushStrLn $ logo ++ "\n" ++ help
help = "Enter ? for help."
helpOptions = 
    "Available commands:\n\
  \  \td: toggles debug mode (on/off)\n\
  \  \t+: insert resource\n\
  \  \t-: remove resource\n\
  \  \tg: change granularity\n\
  \  \tl: load file\n\
  \  \tp: print environment\n\
  \  \ts: start reductions\n\
  \  \tr: reset to initial state\n\
  \  \tq: quit\n\
  \  \t?: help\n"

logo = " \
\ ______     __    __              \n\
\ /_  __/__  / /   / /   ___  _____ \n\
\  / / / _ \\/ /   / /   / _ \\/ ___/ \n\
\ / / /  __/ /___/ /___/  __/ /     \n\
\/_/  \\___/_____/_____/\\___/_/    v0.1   \n"

createEnvFromFile :: FilePath -> IO ProverState
createEnvFromFile fileName = do
    fileContents <- readFile fileName 
    return (changeEnvTo (tts' fileContents) initialState)


-- TODO: restructure the code below




{--
main' = do
     args <- getArgs
     case args of
          [f] -> runFile f
          []  -> runInteractive

runFile f      = readFile f >>= run' term doReductions
runInteractive = getLine    >>= run' term doReductions >> runInteractive


doReductions t = do
  t' <- reduceIO' (map simplify t)
  putStrLn "End of story, no more reductions found for:"
  putStrLn (showTerms t')
--}

