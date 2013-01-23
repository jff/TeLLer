module Main where

import Data.List ((\\))
import Control.Monad.State (lift, evalStateT, get, gets, put, modify, when)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Console.Readline (readline, addHistory)
import Data.Version (showVersion)

-- Import GraphViz to generate graphs
import Data.Graph.Inductive (Gr, mkGraph)
import Data.GraphViz (runGraphviz, graphToDot,nonClusteredParams,GraphvizOutput(..), toLabel, fmtNode, globalAttributes, fmtEdge, isGraphvizInstalled)


-- Build imports
import Paths_teller (version)

-- Local imports
import Parser (parse, term, tts')
import Syntax (Term)
import Reductions (startTeLLer)
import ProverState
import UserIO
import Printer (showTerm)
import CGraph

-- | 'teller_version' is the version number used in the cabal file.
teller_version :: String
teller_version = showVersion version

toggleDebugMode :: ProverStateIO ()
toggleDebugMode = do
    modify changeDebugMode
    inDebugMode <- gets debugMode 
    if inDebugMode then lift (tellerDebug "Debug mode turned on.")
                   else lift (tellerDebug "Debug mode turned off.")

changeGranularity :: ProverStateIO ()
changeGranularity = do
    currentGranularity <- gets granularity 
    g <- lift $ askUserForInt (>0) ("Enter new value for granularity (current value is " ++ (show currentGranularity) ++ "): ")
    modify (changeGranularityTo g)

loadFileAsk :: ProverStateIO ()
loadFileAsk = do
    fileName <- lift $ readFileNameFromUser "Load file: "
    loadFile fileName

loadFile :: FilePath -> ProverStateIO ()
loadFile fileName = do
    fileExists <- lift $ doesFileExist fileName
    if (fileExists) then do fileContents <- lift $ readFile fileName 
                            modify setCountersToZero
                            modify (changeEnvTo (tts' fileContents))
                    else lift (tellerError $ "ERROR: File '" ++ fileName ++ "' does not exist!") 


changeEnvWith :: ([Term] -> ProverState -> ProverState) -> String -> ProverStateIO ()
changeEnvWith f t = do
    case parse term "<interactive>" t of
     Left err -> lift $ tellerError "ERROR: Parsing error. Please try again." 
     Right r  -> modify (f r) 

addResources :: String -> ProverStateIO ()
addResources = changeEnvWith addToEnv

removeResources :: String -> ProverStateIO ()
removeResources = changeEnvWith removeFromEnv

startReductions :: ProverStateIO ()
startReductions = do
    inDebugMode <- gets debugMode
    when inDebugMode $
        do actions <- gets env
           lift $ tellerDebug $ "[DEBUG] ALL ACTIONS: " ++ show actions
    startTeLLer
    treductions <- getNumberTotalReductions
    lift $ tellerWarning $ "End of story, no more reductions found.\n(" ++ (show treductions) ++ " reductions performed)"
    
printState :: ProverState -> IO ()
printState state = tellerPrintLn (showState state)

printGraph :: FilePath -> ProverStateIO ()
printGraph filename = do
    ginstalled <- lift $ isGraphvizInstalled
    when (not ginstalled) $ do
        lift $ tellerWarning "Graphviz is not installed. Please install it and try again."
    when (ginstalled) $ do
        trace <- gets actionTrace
        lift $ tellerWarning (show trace)
        let (nds,eds) = mkCGraph trace
        let cgr = mkGraph nds eds :: Gr String String
        lift $ runGraphviz (graphToDot params cgr) Jpeg filename
        return ()
            -- TODO: move params, etc to CGraph.hs
            where params = nonClusteredParams { 
                            globalAttributes = [],
                            fmtNode = fn,
                            fmtEdge = const []
                           }
                  fn (n,l) = [toLabel l]

mainLoop' :: ProverStateIO ProverState
mainLoop' = do
  state <- get
  let inDebugMode = debugMode state
  when inDebugMode $ lift $ printState state 
  comm <- lift $ readline "Command [cd+-glpsqr?]: "
  lift $ tellerPrint "\n"
  case comm of
    Nothing -> lift $ tellerPrintLn goodbye_msg >> return state
    Just c  -> 
     do lift $ addHistory c 
        continue <- case c of

            -- Causality graph
            ['c']   -> do lift $ tellerWarning $ "The command c requires one argument.\
                                                \ Example: 'c mygraph.jpg' saves the causality graph to the file mygraph.jpg"
                          mainLoop'
            ('c':f) -> printGraph ((head.words) f) >> mainLoop'

            -- Toggle debug mode.
            ('d':_) -> toggleDebugMode >> mainLoop'

            -- Insert resources.
            ['+']   -> do lift $ tellerWarning $ "The command + requires one or more arguments.\
                                                \ Example: '+ A*B A-@C' introduces resources A, B, and A-@C"
                          mainLoop'
            ('+':t) -> addResources t >> mainLoop'
    
            -- Remove resources.
            ['-']   -> do lift $ tellerWarning $ "The command - requires one or more arguments.\
                                                \ Example: '- A*B A-@C' removes resources A, B, and A-@C"
                          mainLoop'
            ('-':t) -> removeResources t >> mainLoop'

            -- Change granularity.
            ('g':_) -> changeGranularity >> mainLoop'

            -- Load file.
            ['l']    -> loadFileAsk >> mainLoop'
            ('l':f) -> loadFile ((head.words) f)  >> mainLoop'

            -- Print current state. If inDebugMode, the state is already being printed, so don't do anything.
            ('p':_) -> if inDebugMode then mainLoop' 
                                      else lift (printState state) >> mainLoop'
            -- Quit.
            ('q':_) -> lift (tellerPrintLn goodbye_msg) >> return state

            -- Reset to the initial state.
            ('r':_) -> lift $ evalStateT mainLoop' initialState --return (Just initialState)

            -- Start reductions.
            ('s':_) -> startReductions >> mainLoop'

            -- Help options.
            ('?':_) -> lift (tellerPrintLn helpOptions) >> mainLoop'

            -- In case the user presses enter, display command line again.
            ('\n':_) -> mainLoop'

            -- All other commands are not recognized.
            _       -> do lift $ tellerPrintLn $ "Command " ++ c ++ " not recognized."
                          mainLoop'
        return state


-- | Main function.
--   TODO: better support for CLI arguments.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> do fileExists <- doesFileExist f
              if(fileExists) then printWelcomeMessage >> createEnvFromFile f >>= evalStateT mainLoop'
                             else printWelcomeMessage >> 
                                  tellerError "ERROR: File passed in command line does not exist." >> 
                                  evalStateT mainLoop' initialState
              return ()
    []  -> do printWelcomeMessage
              evalStateT mainLoop' initialState
              return ()


---------------------------------------------------
-- String constants: welcome message, logo, etc. --
---------------------------------------------------

printWelcomeMessage :: IO ()
printWelcomeMessage = tellerPrintLn $ logo ++ "\nEnter ? for help."

helpOptions :: String
helpOptions = 
    "Available commands:\n\
  \  \tc <filename.jpg>: writes the causality graph to filename.jpg\n\
  \  \td: toggles debug mode (on/off)\n\
  \  \t+ <res>: insert res\n\
  \  \t- <res>: remove res\n\
  \  \tg: change granularity\n\
  \  \tl: load file\n\
  \  \tp: print environment\n\
  \  \ts: start reductions\n\
  \  \tr: reset to initial state\n\
  \  \tq: quit\n\
  \  \t?: help\n"

logo :: String
logo = " \
\ ______     __    __              \n\
\ /_  __/__  / /   / /   ___  _____ \n\
\  / / / _ \\/ /   / /   / _ \\/ ___/ \n\
\ / / /  __/ /___/ /___/  __/ /     \n\
\/_/  \\___/_____/_____/\\___/_/    v" ++ teller_version ++"   \n"

goodbye_msg :: String
goodbye_msg = "Goodbye. Thanks for using TeLLer."
