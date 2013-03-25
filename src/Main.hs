module Main where

import Control.Monad.State (lift, evalStateT, get, gets, put, modify, when)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Console.Readline (readline, addHistory)
import Data.Version (showVersion)
import Data.List (findIndices)

-- Import GraphViz to generate graphs
import Data.Graph.Inductive hiding (version)-- (Gr, mkGraph)
import Data.GraphViz (runGraphviz, graphToDot,GraphvizOutput(..), isGraphvizInstalled)

-- Build imports
import Paths_teller (version)

-- Local imports
import Parser (tts',tt)
import Reductions (startTeLLer)
import ProverState
import UserIO
import CGraph
import Printer


-- | Main function.
--   TODO: better support for CLI arguments.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> do fileExists <- doesFileExist f
              if(fileExists) then printWelcomeMessage >> createEnvFromFile f >>= evalStateT mainLoop
                             else printWelcomeMessage >> 
                                  tellerError "ERROR: File passed in command line does not exist." >> 
                                  evalStateT mainLoop initialState
              return ()
    []  -> do printWelcomeMessage
              evalStateT mainLoop initialState
              return ()

-- | 'mainLoop' is the main interaction loop of the "interpreter". It shows a prompt, it reads a command
--   from the user, executes the command, and then gets back to the beginning (unless the command is "q"uit).
mainLoop :: ProverStateIO ProverState
mainLoop = do
  state <- get
  let inDebugMode = debugMode state
  when inDebugMode $ lift $ printState state 
  comm <- lift $ readline "Command [acd+-glpsxqr?]: "
  lift $ tellerPrint "\n"
  case comm of
    Nothing -> lift $ tellerPrintLn goodbye_msg >> return state
    Just c  -> 
     do lift $ addHistory c 
        continue <- case c of

            -- Toggle debug mode.
            ('a':_) -> toggleTellAllStories >> mainLoop

            -- Causality graph
            ['c']   -> do lift $ tellerWarning $ "The command c requires one argument.\
                                                \ Example: 'c mygraph.jpg' saves the causality graph to the file mygraph.jpg"
                          mainLoop
            ('c':f) -> printGraph ((head.words) f) >> mainLoop

            -- Toggle debug mode.
            ('d':_) -> toggleDebugMode >> mainLoop


            -- Insert resources.
            ['+']   -> do lift $ tellerWarning $ "The command + requires one or more arguments.\
                                                \ Example: '+ A*B A-@C' introduces resources A, B, and A-@C"
                          mainLoop
            ('+':t) -> addResources t >> mainLoop
    
            -- Remove resources.
            ['-']   -> do lift $ tellerWarning $ "The command - requires one or more arguments.\
                                                \ Example: '- A*B A-@C' removes resources A, B, and A-@C"
                          mainLoop
            ('-':t) -> removeResources t >> mainLoop

            -- Change granularity.
            ('g':_) -> changeGranularity >> mainLoop

            -- Load file.
            ['l']    -> loadFileAsk >> mainLoop
            ('l':f) -> loadFile ((head.words) f)  >> mainLoop

            -- Print current state. If inDebugMode, the state is already being printed, so don't do anything.
            ('p':_) -> if inDebugMode then mainLoop 
                                      else lift (printState state) >> mainLoop

            ('x':l) -> checkCounterFactualCausality (words l) >> mainLoop

            -- Quit.
            ('q':_) -> lift (tellerPrintLn goodbye_msg) >> return state

            -- Reset to the initial state.
            ('r':_) -> lift $ evalStateT mainLoop initialState --return (Just initialState)

            -- Start reductions.
            ('s':_) -> startReductions >> mainLoop

            -- Help options.
            ('?':_) -> lift (tellerPrintLn helpOptions) >> mainLoop

            -- In case the user presses enter, display command line again.
            ('\n':_) -> mainLoop

            -- All other commands are not recognized.
            _       -> do lift $ tellerPrintLn $ "Command " ++ c ++ " not recognized."
                          mainLoop
        return state

-- | 'toggleTellAllStories' toggles the tell-all-stories mode
toggleTellAllStories :: ProverStateIO ()
toggleTellAllStories = do
    modify changeTellAllStories
--    inDebugMode <- gets debugMode 
    tellAllStories <- gets tellAllStories
    if tellAllStories then lift (tellerDebug "TeLLer will now tell all possible stories.")
                      else lift (tellerDebug "TeLLer will now tell only one story.")


-- | 'toggleDebugMode' switches on or off the debug mode
toggleDebugMode :: ProverStateIO ()
toggleDebugMode = do
    modify changeDebugMode
    inDebugMode <- gets debugMode 
    if inDebugMode then lift (tellerDebug "Debug mode turned on.")
                   else lift (tellerDebug "Debug mode turned off.")

-- | 'changeGranularity' can be used to change the granularity of focusing.
--   It asks a positive integer from the user.
changeGranularity :: ProverStateIO ()
changeGranularity = do
    currentGranularity <- gets granularity 
    g <- lift $ askUserForInt (>0) ("Enter new value for granularity (current value is " ++ (show currentGranularity) ++ "): ")
    modify (changeGranularityTo g)

-- | 'loadFileAsk' reads a file name from the user and loads the file,
--   i.e., it changes the environment to the resources and actions described in the file.
loadFileAsk :: ProverStateIO ()
loadFileAsk = do
    fileName <- lift $ readFileNameFromUser "Load file: "
    loadFile fileName

-- | 'loadFile' loads the file given as a parameter, i.e.,
--   it changes the environment to the resources and actions described in the file.
loadFile :: FilePath -> ProverStateIO ()
loadFile fileName = do
    fileExists <- lift $ doesFileExist fileName
    if (fileExists) then do fileContents <- lift $ readFile fileName 
                            modify setCountersToZero
                            modify (changeEnvTo (tts' fileContents))
                    else lift (tellerError $ "ERROR: File '" ++ fileName ++ "' does not exist!") 

-- | 'addResources' adds the resources specified in the argument (as a string) to the environment.
addResources :: String -> ProverStateIO ()
addResources = changeEnvWith addToEnv

-- | 'removeResources' removes the resources specified in the argument (as a string) from the environment.
removeResources :: String -> ProverStateIO ()
removeResources = changeEnvWith removeFromEnv

-- | 'startReductions' starts the forward chaining process with the resources currently available in
--   the environment.
startReductions :: ProverStateIO ()
startReductions = do
    inDebugMode <- gets debugMode
    when inDebugMode $
        do actions <- gets env
           lift $ tellerDebug $ "[DEBUG] ALL ACTIONS: " ++ show actions
    startTeLLer
    treductions <- getNumberTotalReductions
    lift $ tellerWarning $ "End of story, no more reductions found.\n(" ++ (show treductions) ++ " reductions performed)"
    
-- | 'printGraph' creates a JPEG image of the causality graph and saves it to the file name given
--   as argument.
printGraph :: FilePath -> ProverStateIO ()
printGraph filename = do
    ginstalled <- lift $ isGraphvizInstalled
    when (not ginstalled) $ do
        lift $ tellerWarning "Graphviz is not installed. Please install it and try again."
    when (ginstalled) $ do
        trace <- gets actionTrace
        let (nds,eds) = mkCGraph trace
        let cgr = mkGraph nds eds :: Gr String String
        lift $ runGraphviz (graphToDot cGraphParams cgr) Pdf filename
        lift $ runGraphviz (graphToDot cGraphParams cgr) DotOutput (filename++".dot")

        tellAllStories <- gets tellAllStories
        when(tellAllStories) $ do
            {-- EXPERIMENTAL: generate all graphs --}
            allTraces <- gets btTraces
            let allGraphs = map ((uncurry mkGraph) . mkCGraph) allTraces :: [Gr String String]
--            let a = showTerm $ tt "a-@c*b"
--            let b = showTerm $ tt "b*c-@d"
    --        lift $ putStrLn $ show $ map ((flip findNodeId) ("_o_"++a)) allGraphs
--            lift $ putStrLn $ show $ map (linkExists a b) allGraphs
            let numbered = zip [0..] allGraphs 
            let producePDF (n,g) = runGraphviz (graphToDot cGraphParams g) Pdf ('_':(show n)++".pdf")
            lift $ sequence_ $ map producePDF numbered

        return ()

checkCounterFactualCausality :: [String] -> ProverStateIO ()
checkCounterFactualCausality l = do
    if (length l < 2) then do
        lift $ tellerWarning "The command 'x' takes two actions as arguments."
                      else do
        let string_a1 = l!!0
        let string_a2 = l!!1
        -- TODO: in the future, allow more than two actions as arguments, i.e., is a1 -> [a2,a3,a4] ?
        let a1 = showTerm $ tt string_a1
        let a2 = showTerm $ tt string_a2
        allTraces <- gets btTraces
        let allGraphs = map ((uncurry mkGraph) . mkCGraph) allTraces :: [Gr String String]
        let allChecks = map (linkExists a1 a2) allGraphs
        if (and allChecks) then lift $ tellerPrintLn $ "Yes: " ++ a2 ++ " is caused by " ++ a1 ++ " in *all* the possible narratives!"
                           else do
                                    let indices = findIndices (==False) allChecks 
                                    let counterexamples = map ((\s->('_':s)++".pdf ").show) indices
                                    lift $ tellerPrint $ "No: " ++ a2 ++ " is not caused by " ++ a1 ++ " in the following narratives: "
                                    lift $ sequence_ $ map tellerPrint counterexamples
                                    lift $ tellerPrintLn "" -- add new line

-- | 'printState' prints the state given as argument. The output is defined by
--   the function 'showState'.
printState :: ProverState -> IO ()
printState state = tellerPrintLn (showState state)

-- | 'teller_version' is the version number used in the cabal file.
teller_version :: String
teller_version = showVersion version

---------------------------------------------------
-- String constants: welcome message, logo, etc. --
---------------------------------------------------

printWelcomeMessage :: IO ()
printWelcomeMessage = tellerPrintLn $ logo ++ "\nEnter ? for help."

helpOptions :: String
helpOptions = 
    "Available commands:\n\
  \  \ta: tell all the possible stories (and generate all the graphs)\n\
  \  \tc <filename.jpg>: writes the causality graph to filename.jpg\n\
  \  \td: toggles debug mode (on/off)\n\
  \  \t+ <res>: insert res\n\
  \  \t- <res>: remove res\n\
  \  \tg: change granularity\n\
  \  \tl: load file\n\
  \  \tp: print environment\n\
  \  \ts: start reductions\n\
  \  \tx <a1> <a2>: checks if action <a2> is caused by <a1> in all the generated graphs\n\
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
