{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

-- Import GraphViz to generate graphs
import Data.Graph.Inductive (Gr, mkGraph)
import Data.GraphViz (runGraphviz, graphToDot,GraphvizOutput(..), isGraphvizInstalled)

import System.IO (hPutStrLn, stderr, stdin, openFile, IOMode(..), hClose, hGetContents)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)
import System.Console.Readline (readline, addHistory)
import System.Process (readProcessWithExitCode)
import System.Exit

import Control.Monad.State -- (lift, evalStateT, get, gets, put, modify, when)
import qualified Data.Map as Map -- hiding (null, foldr)
import Control.Applicative
import Data.Maybe (fromJust, fromMaybe)
import Data.List (nub, sort, groupBy, (\\), partition, findIndices)

import Text.ParserCombinators.Parsec hiding (State)

-- Local imports
import CelfToGraphParser
import CelfToGraphConf
import Syntax
import Parser 
import Term (detensor, deWith)
import CLI
import Printer
import CausalityGraph hiding (getActionName)


------------------------------------------------------------------
-- Data structures
------------------------------------------------------------------

data CState = State {
    traces :: Traces,
    graphs :: [Gr String String]
} deriving Show

initialState = State {
    traces = Trace [] [],
    graphs = []
}

type CStateIO a = StateT CState IO a

------------------------------------------------------------------
-- Code that converts CelfOutput to Traces
------------------------------------------------------------------

celfoutToTraces :: CelfOutput -> Traces
celfoutToTraces (Celf init actions solutions) = Trace init $ filter (\l -> length l > 0)$ map (getTrace init actions) solutions

getTrace :: [Term] -> Map.Map String Term -> Solution -> ActionTrace
getTrace init actions (Lambda _ ls) = concatMap (getAction actions nameBindings) ls
    where nameBindings = createNameBindings ls Map.empty
          createNameBindings [] currentBindings = currentBindings
          createNameBindings ((Let vs Nothing _):xs) currentBindings = 
            let rs = concatMap detensor init
                newBindings = Map.fromList $ zip vs rs
            in createNameBindings xs (currentBindings `Map.union` newBindings)
          createNameBindings ((Let vs (Just a) rs):xs) currentBindings = 
            let actionName = getActionName a actions currentBindings
                newResources = detensor $ getLeftLolli $ fromMaybe (Atom ("ERROR: action " ++ actionName)) $ Map.lookup actionName actions
                newBindings = Map.fromList $ zip vs newResources 
            in createNameBindings xs (currentBindings `Map.union` newBindings)

getAction :: Map.Map String Term -> Map.Map String Term -> LetBinding -> [Term]
getAction actions currentBindings (Let _ Nothing _) = []
getAction actions currentBindings (Let _ (Just name) _) = 
    let actionName = getActionName name actions currentBindings --[fromMaybe (Atom "ERROR") (Map.lookup name actions)]
    in [fromMaybe (Atom ("*ERROR: " ++ actionName)) (Map.lookup actionName actions)]

getActionNameFromBindings a currentBinds =
    let ruleNameGarbage = show $ fromMaybe (Atom ("ERROR: action" ++ a)) ((Map.lookup a currentBinds) `mplus` (Map.lookup ('!':a) currentBinds))
        stripActionName a | ((take 8 a) == "OfCourse") = removeAtomAndQuotes (drop 9 a)
                          | otherwise                  = removeAtomAndQuotes a
        removeAtomAndQuotes = filter (not . (\c -> c `elem` ['\\','"', ')', '('])) . (drop 6) 
        ruleName = stripActionName ruleNameGarbage
    in ruleName

getActionNameFromProjection a  currentBinds =
    -- The action name is of the form #N_Var; we need to get the term associated with Var, and extract the name of the Nth projection,
    let (projection, var) = (\(x,y) -> (read $ drop 1 x :: Int, drop 1 y)) . span (/='_') $  a
        allRuleNameGarbage = fromMaybe (Atom ("ERROR: action" ++ a)) (Map.lookup var currentBinds)
        -- Atom "a" :&: Atom "b" :&: etc...
        allChoices = deWith allRuleNameGarbage -- transforms argument into list
        removeAtomAndQuotes = filter (not . (\c -> c `elem` ['\\','"'])) . (drop 6) 
        ruleName = removeAtomAndQuotes (show (allChoices!!(projection-1)))
    in ruleName


getActionName a actions currentBinds =
    if Map.member a actions 
    then a 
    else if (head a == '#') -- it's a projection... 
         then getActionNameFromProjection a currentBinds
         else getActionNameFromBindings a currentBinds

-- End of code that converts CelfOutput to Traces

------------------------------------------------------------------
--  Functions that parse the output of Celf
------------------------------------------------------------------
parseString :: String -> CelfOutput
parseString str =
  case parse celfOutputParser "" str of
    Left e  -> error $ show e
    Right r -> r

------------------------------------------------------------------
--  Functions that interact with the user and change the state
------------------------------------------------------------------
-- | 'loadFileAsk' reads a file name from the user and loads the file,
--   i.e., it changes the environment to the resources and actions described in the file.
loadFileAsk :: CStateIO ()
loadFileAsk = do
    fileName <- lift $ readFileNameFromUser "Load file: "
    loadFile fileName

-- | 'loadFile' loads the file given as a parameter, i.e.,
--   it changes the environment to the resources and actions described in the file.
loadFile :: FilePath -> CStateIO ()
loadFile fileName = do
    fileExists <- lift $ doesFileExist fileName
    if (fileExists) then do fileContents <- lift $ readFile fileName 
--                            out <- lift $ readProcess celf_cmd [fileName] ""
                            (exitcode, out, err) <- lift $ readProcessWithExitCode celf_cmd [fileName] ""
                            case exitcode of
                                ExitSuccess -> do
                                                let celfOut = parseString out
                                                let newTraces = celfoutToTraces celfOut
                                                let newGraphs = graphsFromTraces newTraces
                                                modify (\state -> state {traces = newTraces, graphs = newGraphs})
                                                lift $ tellerPrintLn $ "Done. " ++ show (length newGraphs) ++ " graphs generated."
                                (ExitFailure e) -> do
                                                    lift $ tellerPrintLn $ "An error occurred when running celf on the file provided. \
                                                                          \ Error code: "++show e++".\n" ++ err
                    else lift (tellerError $ "ERROR: File '" ++ fileName ++ "' does not exist!") 



-- | 'writeGraphsToDir' writes all graphs to the directory given
writeGraphsToDir :: FilePath -> CStateIO ()
writeGraphsToDir dirName = do
    dirExists <- lift $ doesDirectoryExist dirName
    if (dirExists) then lift $ tellerPrintLn "Error: directory exists."
                   else do lift $ createDirectory dirName
                           -- Write graphs
                           graphs <- gets graphs
                           let createPDF filename g = runGraphviz (graphToDot cGraphParams g) Pdf filename
                           let ioCommands = zipWith ($) [createPDF (dirName++"/"++((show n)++".pdf")) | n<-[0..]] graphs
                           lift $ sequence_ ioCommands
                           -- Write traces
                           traces <- gets traces
                           let ioCommands = zipWith ($) [writeFile (dirName++"/"++((show n)++".trace")) | n<-[0..]] (showActionTraces traces)
                           lift $ sequence_ ioCommands
                           lift $ tellerPrintLn $ "Done. " ++ show (length graphs) ++ " graphs written to directory " ++ dirName ++ "."
 
checkCounterFactualCausality :: [String] -> CStateIO ()
checkCounterFactualCausality l = do
    if (length l < 2) then do
        lift $ tellerWarning "The command 'link' takes two actions as arguments."
                      else do
        let string_a1 = l!!0
        let string_a2 = l!!1
        -- TODO: in the future, allow more than two actions as arguments, i.e., is a1 -> [a2,a3,a4] ?
        let a1 = showTerm $ tt string_a1
        let a2 = showTerm $ tt string_a2
        allGraphs <- gets graphs
        if (null allGraphs) then lift $ tellerPrintLn $ "You need to load a file before querying the graphs."
                            else do
            let allChecks = map (linkExists a1 a2) allGraphs
            if (and allChecks) then lift $ tellerPrintLn $ "Yes: " ++ a2 ++ " is caused by " ++ a1 ++ " in all the generated narratives!"
                               else do
                                let indicesTrue = findIndices (==True) allChecks 
                                let indicesFalse = findIndices (==False) allChecks 
                                let examples = map ((\s->('_':s)++".pdf ").show) indicesTrue
                                let counterexamples = map ((\s->('_':s)++".pdf ").show) indicesFalse
                                lift $ tellerPrint $ "No: " ++ a2 ++ " is not caused by " ++ a1 ++ " in the following narratives: "
                                lift $ sequence_ $ map tellerPrint counterexamples
                                when (not (null examples)) $ do
                                    lift $ tellerPrint $ "Yes: " ++ a2 ++ " is caused by " ++ a1 ++ " in the following narratives: "
                                    lift $ sequence_ $ map tellerPrint examples
                                lift $ tellerPrintLn "" -- add new line

main :: IO ()
main = do
  printWelcomeMessage
  evalStateT mainLoop initialState
  return ()

mainLoop :: CStateIO CState
mainLoop = do
  state <- get
  comm <- lift $ readline "Command [w, link, l, q, ?]: "
  lift $ tellerPrint "\n"
  case comm of
    Nothing -> lift $ putStrLn goodbye_msg >> return state
    Just c  -> 
     do lift $ addHistory c 
        continue <- case c of


            -- Write to folder
            ('w':f) -> writeGraphsToDir ((head.words) f)  >> mainLoop

            -- Link from a1 to a2
            ('l':'i':'n':'k':as) -> checkCounterFactualCausality (words as) >> mainLoop

            -- Load file.
            ['l']    -> loadFileAsk >> mainLoop
            ('l':f) -> loadFile ((head.words) f)  >> mainLoop

            -- Quit.
            ('q':_) -> lift (putStrLn goodbye_msg) >> return state

            -- Help options.
            ('?':_) -> lift (tellerPrintLn helpOptions) >> mainLoop

            -- All other commands are not recognized.
            _       -> do lift $ putStrLn $ "Command " ++ c ++ " not recognized."
                          mainLoop
        return state

printWelcomeMessage :: IO ()
printWelcomeMessage = tellerPrintLn $ logo ++ "\nEnter ? for help."

helpOptions :: String
helpOptions = 
    "Available commands:\n\
  \  \tw <foldername>: writes the causality graphs to folder <foldername>\n\
  \  \tl <filename>: load file <filename>\n\
  \  \tl: ask for file name and load it\n\
  \  \tlink <a1> <a2>: checks if action <a2> is caused by <a1> in the generated graphs\n\
  \  \tq: quit\n\
  \  \t?: help\n"

logo :: String
logo = " \
\ _____     _  __ _______     _____                 _     \n\
\ / ____|   | |/ _|__   __|   / ____|               | |    \n\
\| |     ___| | |_   | | ___ | |  __ _ __ __ _ _ __ | |__  \n\
\| |    / _ \\ |  _|  | |/ _ \\| | |_ | '__/ _` | '_ \\| '_ \\ \n\
\| |___|  __/ | |    | | (_) | |__| | | | (_| | |_) | | | |\n\
\ \\_____\\___|_|_|    |_|\\___/ \\_____|_|  \\__,_| .__/|_| |_|\n\
\                                             | |          \n\
\                                             |_|  \n"

goodbye_msg :: String
goodbye_msg = "Goodbye. Thanks for using CelfToGraph."
