{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

-- Import GraphViz to generate graphs
import Data.Graph.Inductive -- (equal, Gr, labNodes) --(Gr, mkGraph, equal)
import Data.Graph.Inductive.Query.BFS (level)
import Data.GraphViz (runGraphviz, graphToDot,GraphvizOutput(..), isGraphvizInstalled)

import System.IO (hPutStrLn, stderr, stdin, openFile, IOMode(..), hClose, hGetContents)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory, removeDirectoryRecursive)
import System.Process (readProcessWithExitCode)
import System.Exit

import Control.Monad.State -- (lift, evalStateT, get, gets, put, modify, when)
import qualified Data.Map as Map -- hiding (null, foldr)
import Control.Applicative
import Data.Maybe (fromJust, fromMaybe)
import Data.List (nub, sort, groupBy, (\\), partition, findIndices, findIndex)

import Text.ParserCombinators.Parsec hiding (State)

-- Local imports
import CelfToGraphParser
import CelfToGraphConf
import Syntax
import Parser 
import Term (detensor, deWith)
import UserIO
import Printer
import CausalityGraph hiding (getActionName)
import CausalityGraphQueries


------------------------------------------------------------------
-- Data structures
------------------------------------------------------------------

data CState = State {
    traces :: Traces,
    graphs :: [Gr String String],
    fileName :: String
} deriving Show

initialState = State {
    traces = Trace [] [],
    graphs = [],
    fileName = ""
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
    if ((length fileName)>0) then loadFile fileName
                             else lift (putStrLn "Please provide a valid file name.") >> return ()

-- | 'loadFile' loads the file given as a parameter, i.e.,
--   it changes the environment to the resources and actions described in the file.
loadFile :: FilePath -> CStateIO ()
loadFile fileName = do
    fileExists <- lift $ doesFileExist fileName
    if (fileExists) then do fileContents <- lift $ readFile fileName 
                            modify (\state -> state {fileName = fileName})
--                            out <- lift $ readProcess celf_cmd [fileName] ""
                            (exitcode, out, err) <- lift $ readProcessWithExitCode celf_cmd [fileName] []
                            case exitcode of
                                ExitSuccess -> do
                                                let celfOut = parseString out
                                                let newTraces = celfoutToTraces celfOut
                                                let newGraphs = graphsFromTraces newTraces
                                                modify (\state -> state {traces = newTraces, graphs = newGraphs})
                                                lift $ tellerPrintLn $ "Done. " ++ show (length newGraphs) ++ " graphs generated."
                                                let Trace _ actTraces = newTraces
                                                let nonDupActTraces = nub actTraces
                                                let allDifferent = nonDupActTraces == actTraces
                                                let duplicateActTraces = actTraces \\ nonDupActTraces
                                                let indicesDuplicates = findIndices (\e -> e `elem` duplicateActTraces) actTraces
                                                if(allDifferent) then lift $ tellerPrintLn $ "(All traces are different.)"
                                                                 else lift $ tellerPrintLn $ "(Not all traces are different: " ++ show indicesDuplicates ++ ")"
                                (ExitFailure e) -> do
                                                    lift $ tellerPrintLn $ "An error occurred when running celf on the file provided. \
                                                                          \ Error code: "++show e++".\n" ++ err ++ out
                    else lift (tellerError $ "ERROR: File '" ++ fileName ++ "' does not exist!") 

-- | 'reloadFile' reloads the file given by the fileName variable stored in the state
reloadFile :: CStateIO ()
reloadFile = do
   fileName <- gets fileName
   loadFile fileName 

openGraph :: [String] -> CStateIO ()
openGraph graphs = do
    let error_message = "Please pass the number of the graph as argument."
    if(length graphs < 1) then lift $ tellerError error_message
                          else   do
                               let arg = head graphs
                               let n = reads arg :: [(Int, String)]
                               case n of
                                 []      -> lift $ tellerError error_message
                                 (i,_):_ -> do
                                              let runtime_dir = ".temp_runtime_celf2graphs"
                                              runtime_dir_exists <- lift $ doesDirectoryExist runtime_dir
                                              when (runtime_dir_exists) $ lift $ removeDirectoryRecursive runtime_dir
                                              writeOnlyOneGraphToDir runtime_dir i
                                              -- TODO: delete the temporary dir (and improve code to generate just one graph)
                                              _ <- lift $ readProcessWithExitCode "/usr/bin/open" [runtime_dir++"/"++(show i)++".pdf"] []
                                              -- TODO: make this open call platform-independent
                                              return ()

-- | 'showStats' displays some statistics
showStats = do
    traces <- gets traces
    graphs <- gets graphs
    let numGraphs = length graphs
    let uniqueGraphs = nub graphs
    let numUniqueGraphs = length uniqueGraphs
    lift $ tellerPrint $ "Number of graphs generated:\t" ++ show numGraphs
    if(numUniqueGraphs < numGraphs) -- some duplicates
     then do let duplicateGraphs = graphs \\ uniqueGraphs
             let indicesUniqueGraphs = map (fromMaybe (-1)) $ (map findIndex (map (==) uniqueGraphs)) <*> [graphs]
             let indicesDuplicateGraphs = [0..numGraphs-1] \\ indicesUniqueGraphs
             lift $ tellerPrintLn $ "\n\tNumber of unique graphs:\t"     ++ show numUniqueGraphs ++ "\t("++show indicesUniqueGraphs++")"
             lift $ tellerPrintLn $ "\tNumber of duplicated graphs:\t" ++ show (numGraphs-numUniqueGraphs)++ "\t("++show indicesDuplicateGraphs++")"
     else lift $ tellerPrintLn $ "\t(all of them different)"
    let Trace _ actTraces = traces
    let nonDupActTraces = nub actTraces
    let allDifferent = nonDupActTraces == actTraces
    let duplicateActTraces = actTraces \\ nonDupActTraces
    let indicesDuplicates = findIndices (\e -> e `elem` duplicateActTraces) actTraces
    if(allDifferent) then lift $ tellerPrintLn $ "All the traces are different."
                     else lift $ tellerPrintLn $ "Not all traces are different: " ++ show indicesDuplicates 

--TODO: FIXME! Graph.Inductive now has an Eq definition; make sure it is okay...
{--
instance (Eq a, Ord a, Eq b) => Eq (Gr a b) where
    g1 == g2 = g1 `myEquals` g2
     where
        myEquals g1 g2 =
            let l1 = graphToLevelSortedList g1
                l2 = graphToLevelSortedList g2
            in l1==l2

        graphToLevelSortedList g = 
            let levels = level 0 g -- [(NodeId, Level)]
                groupedLevels = groupBy (\p1 p2 -> snd p1 == snd p2) levels -- [[(NodeId,Level)]]
                unzippedLevelsWithNames = map ((\p -> (sort (map (getNodeLabel g) (fst p)), head (snd p))) . unzip) groupedLevels
            in unzippedLevelsWithNames

        getNodeLabel g nodeid =
            let maybeName = lookup nodeid (labNodes g)
            in  fromJust maybeName
--}


-- | 'writeGraphsToDir' writes all graphs to the directory given
writeGraphsToDir :: FilePath -> CStateIO ()
writeGraphsToDir dirName = do
    dirExists <- lift $ doesDirectoryExist dirName
    if (dirExists) then lift $ tellerPrintLn "Error: directory exists."
                   else do lift $ createDirectory dirName
                           -- Write graphs
                           graphs <- gets graphs
                           let createGVFile filename filetype g = runGraphviz (graphToDot cGraphParams g) filetype filename
                           let ioCommands = zipWith ($) [createGVFile (dirName++"/"++((show n)++".pdf")) Pdf | n<-[0..]] graphs
                           lift $ sequence_ ioCommands
                           -- Write .dot files
                           let ioCommands = zipWith ($) [createGVFile (dirName++"/"++((show n)++".dot")) DotOutput | n<-[0..]] graphs
                           lift $ sequence_ ioCommands
                           -- Write traces
                           traces <- gets traces
                           let ioCommands = zipWith ($) [writeFile (dirName++"/"++((show n)++".trace")) | n<-[0..]] (showActionTraces traces)
                           lift $ sequence_ ioCommands
                           lift $ tellerPrintLn $ "Done. " ++ show (length graphs) ++ " graphs written to directory " ++ dirName ++ "."

-- | 'writeOnlyOneGraphToDir' writes the graph given as argument to
--   the directory given (if the dir exists, it writes the graph anyway)
--  This function is used by the "show" command
writeOnlyOneGraphToDir :: FilePath -> Int -> CStateIO ()
writeOnlyOneGraphToDir dirName gnumber = do
    dirExists <- lift $ doesDirectoryExist dirName
    when (not dirExists) $ lift $ createDirectory dirName
    -- Write graph
    graphs <- gets graphs
    if (gnumber >= length graphs || gnumber < 0) 
        then lift $ tellerError "ERROR: Invalid graph number."
        else do
                let graphToBeWritten = graphs!!gnumber -- This is safe due to the conditional test above
                let createGVFile filename filetype g = runGraphviz (graphToDot cGraphParams g) filetype filename
                lift $ createGVFile (dirName++"/"++((show gnumber)++".pdf")) Pdf graphToBeWritten
                return ()
--                let ioCommands = zipWith ($) [createGVFile (dirName++"/"++((show n)++".pdf")) Pdf | n<-[0..]] graphs
--                           lift $ sequence_ ioCommands
                           -- Write .dot files
--                           let ioCommands = zipWith ($) [createGVFile (dirName++"/"++((show n)++".dot")) DotOutput | n<-[0..]] graphs
--                           lift $ sequence_ ioCommands
                           -- Write traces
--                           traces <- gets traces
--                           let ioCommands = zipWith ($) [writeFile (dirName++"/"++((show n)++".trace")) | n<-[0..]] (showActionTraces traces)
--                           lift $ sequence_ ioCommands
--                           lift $ tellerPrintLn $ "Done. " ++ show (length graphs) ++ " graphs written to directory " ++ dirName ++ "."
 
 

-- remove words ws from list of words lws
removeWords ws lws = filter (\w -> not(w `elem` ws)) lws

processQuery :: CGQuery -> CStateIO ()
processQuery query = do
    allGraphs <- gets graphs
    if (null allGraphs) then lift $ tellerPrintLn $ "You need to load a file before querying the graphs."
                        else do
            let (queryResult, indicesTrue) = checkBooleanQuery query allGraphs
            let numGraphs = length allGraphs
            let allValid = numGraphs == length indicesTrue
            if (queryResult && allValid) then lift $ tellerPrintLn $ "Yes: the query is valid in all the generated narratives!"
                               else do
                                let indicesFalse = [0..numGraphs-1]\\indicesTrue
                                let examples = map ((\s->('_':s)++".pdf ").show) indicesTrue
                                let counterexamples = map ((\s->('_':s)++".pdf ").show) indicesFalse
                                if (length counterexamples == numGraphs) 
                                 then lift $ tellerPrintLn $ "No: there are no graphs satisfying the query."
                                 else do
                                  lift $ tellerPrint $ "No: the query is not valid in the following narratives: "
                                  lift $ sequence_ $ map tellerPrint counterexamples
                                  lift $ tellerPrintLn "\n" -- add 2 new lines
                                when (not (null examples)) $ do
                                    lift $ tellerPrint $ "\nYes: the query is valid in the following narratives: "
                                    lift $ sequence_ $ map tellerPrint examples
                                lift $ tellerPrintLn "\n" -- add 2 new lines


    
{--
checkCounterFactualCausality :: [String] -> CStateIO ()
checkCounterFactualCausality ws = do
    let l = removeWords ["and", "link"] ws
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

--}

main :: IO ()
main = do
  printWelcomeMessage
  evalStateT mainLoop initialState
  return ()


mainLoop :: CStateIO CState
mainLoop = do
  state <- get
  comm <- lift $ prompt "Command [l, r, stats, link, exists, w, q, ?]: "
  lift $ tellerPrint "\n"
  case comm of
    Nothing -> lift $ putStrLn goodbye_msg >> return state
    Just c  -> 
     do lift $ addHistory c 
        continue <- case (parse queryParser "" c) of
            Right query -> processQuery query >> mainLoop
            Left e -> do 
                --lift $ putStrLn $ show e 
                case c of

                    -- Write to folder
                    ['w'] -> lift (putStrLn "The command w requires an argument. Please try again.") >> mainLoop
                    ('w':f) -> writeGraphsToDir ((head.words) f)  >> mainLoop

                    -- Link from a1 to a2
        --            ('l':'i':'n':'k':as) -> checkCounterFactualCausality (words as) >> mainLoop

                    -- Load file.
                    ['l']    -> loadFileAsk >> mainLoop
                    ('l':f) -> loadFile ((head.words) f)  >> mainLoop

                    -- Load file.
                    ['r']       -> reloadFile >> mainLoop
                    "reload"    -> reloadFile >> mainLoop


                    ('s':'h':'o':'w':n)       -> openGraph (words n) >> mainLoop
            
                    -- Display some statistics (no. of graphs, unique graphs, etc.)
                    "stats" -> showStats >> mainLoop

                    -- Quit.
                    ('q':_) -> lift (putStrLn goodbye_msg) >> return state

                    -- Help options.
                    ('?':_) -> lift (tellerPrintLn helpOptions) >> mainLoop

                    -- All other commands are not recognized.
                    _       -> do lift $ putStrLn $ "Command " ++ c ++ " not recognized."
                                  mainLoop
        return state

printWelcomeMessage :: IO ()
printWelcomeMessage = tellerPrintLn $ logo ++ "\nEnter ? for help and available commands."

helpOptions :: String
helpOptions = 
    "Available commands:\n\
  \  \tl <filename>: loads Celf file <filename> and generates structured graphs\n\
  \  \tl: asks for Celf file name, loads it, and generates structured graphs\n\
  \  \tr: reloads the last file to be loaded\n\
  \  \tw <foldername>: writes the structured graphs to folder <foldername>\n\
  \  \texists <a>: checks if action <a> exists in the generated graphs\n\
  \  \tlink <a1> <a2>: checks if action <a2> is caused by <a1> in the generated graphs\n\
  \  \tstats: shows how many graphs and traces are unique and how many are duplicated\n\
  \  \tq: quit\n\
  \  \t?: help\n\
  \  \n\
  \  \tNOTE: The commands 'exists' and 'link' can be combined with the boolean operators:\n\
  \  \t      ~, &&, ||, <=, =>, and <=>.\n\
  \  \tFor example: \n\
  \  \t      link a1 a2  <=  exists a3 && exists a4\n"

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
