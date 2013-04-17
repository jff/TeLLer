{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

-- Import GraphViz to generate graphs
import Data.Graph.Inductive (Gr, mkGraph)
import Data.GraphViz (runGraphviz, graphToDot,GraphvizOutput(..), isGraphvizInstalled)


import System.IO (hPutStrLn, stderr, stdin, openFile, IOMode(..), hClose, hGetContents)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)
import System.Console.Readline (readline, addHistory)
import System.Process (readProcess)

import Control.Monad.State -- (lift, evalStateT, get, gets, put, modify, when)
import qualified Data.Map as Map -- hiding (null, foldr)
import Control.Applicative
import Data.Maybe (fromJust, fromMaybe)
import Data.List (nub, sort, groupBy, (\\), partition, findIndices)

import Text.ParserCombinators.Parsec hiding (State)

-- Local imports
import CelfToGraphParser
import Syntax
import Parser 
import Term (detensor)
import CLI
import Printer

-- Local imports
import CGraph

type Trace = [(Int,[Int],String)]
data CState = State {
    nameBinds         :: Map.Map String (Term,Int), -- the int is the nodeid
    originOfResources :: Map.Map Term [Int],
    trace :: Trace, -- current trace
    nodeId :: Int,
    allTraces :: [Trace]
} deriving Show

initialState = State {
    nameBinds = Map.empty,
    originOfResources = Map.empty,
    trace = [],
    nodeId = 0,
    allTraces = []
}

type CStateIO a = StateT CState IO a

parseString :: String -> CelfOutput
parseString str =
  case parse celfOutputParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO CelfOutput
parseFile file =
   do celfout  <- readFile file
      case parse celfOutputParser "" celfout of
        Left e  -> print e >> fail "parse error"
        Right r -> return r

-- | 'loadFileAsk' reads a file name from the user and loads the file,
--   i.e., it changes the environment to the resources and actions described in the file.
loadFileAsk :: CStateIO ()
loadFileAsk = do
    fileName <- lift $ readFileNameFromUser "Load file: "
    loadFile fileName

celf_cmd = "/Users/jff/work/phd-repository/code/celf/celf/celf"

-- | 'loadFile' loads the file given as a parameter, i.e.,
--   it changes the environment to the resources and actions described in the file.
loadFile :: FilePath -> CStateIO ()
loadFile fileName = do
    fileExists <- lift $ doesFileExist fileName
    if (fileExists) then do fileContents <- lift $ readFile fileName 
                            out <- lift $ readProcess celf_cmd [fileName] ""
                            let f = parseString out
                            modify (processSolutions f)
                            lift $ tellerPrintLn "Done."
                    else lift (tellerError $ "ERROR: File '" ++ fileName ++ "' does not exist!") 

-- | 'writeGraphsToDir' writes all graphs to the directory given
writeGraphsToDir :: FilePath -> CStateIO ()
writeGraphsToDir dirName = do
    dirExists <- lift $ doesDirectoryExist dirName
    if (dirExists) then lift $ tellerPrintLn "Error: directory exists."
                   else do lift $ createDirectory dirName
                           ts <- gets allTraces
                           let gs = map mkCGraph ts -- gs=[(nds,eds)]
                           let graphs = map (uncurry mkGraph) gs :: [Gr String String]
                           let createPDF filename g = runGraphviz (graphToDot cGraphParams g) Pdf filename
                           let ioCommands = zipWith ($) [createPDF (dirName++"/"++((show n)++".pdf")) | n<-[0..]] graphs
                           lift $ sequence_ ioCommands
                           lift $ tellerPrintLn $ "Done. Number of graphs generated: " ++ show (length graphs)
 
main :: IO ()
main = do
  printWelcomeMessage
  evalStateT mainLoop initialState
  return ()

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
        allTraces <- gets allTraces
        let allGraphs = map ((uncurry mkGraph) . mkCGraph) allTraces :: [Gr String String]
        let allChecks = map (linkExists a1 a2) allGraphs
        if (and allChecks) then lift $ tellerPrintLn $ "Yes: " ++ a2 ++ " is caused by " ++ a1 ++ " in *all* the possible narratives!"
                           else do
                                    let indices = findIndices (==False) allChecks 
                                    let counterexamples = map ((\s->('_':s)++".pdf ").show) indices
                                    lift $ tellerPrint $ "No: " ++ a2 ++ " is not caused by " ++ a1 ++ " in the following narratives: "
                                    lift $ sequence_ $ map tellerPrint counterexamples
                                    lift $ tellerPrintLn "" -- add new line



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

-- TODO: parameterize the number of nodes
addInitialResources :: CelfOutput -> CState -> CState
addInitialResources (Celf init _ _) state = 
    let rs = concatMap detensor init
        newMap = foldr (\k -> Map.insertWith (++) k [0]) (originOfResources state) rs
    in
    state { originOfResources = newMap }


processSolutions :: CelfOutput -> CState -> CState
processSolutions (Celf init actions []) state = state
processSolutions o@(Celf init actions (l:ls)) state = 
    let state' = addInitialResources o state
        newState = processLambda init actions l state'
        lastTrace = trace newState
        previousTraces = allTraces newState
        updatedState = initialState { allTraces = previousTraces ++ (filter (not . null) [lastTrace]), trace = [], nodeId = 0 }
    in processSolutions (Celf init actions ls) updatedState

processLambda init actions (Lambda _ ls) state = processLetBindings ls state
    where processLetBindings [] state = state
          processLetBindings ((Let vs Nothing _):xs) state = 
            let rs = zip (concatMap detensor init) (repeat 0)
                bs = Map.fromList $ zip vs rs
                oldBinds = nameBinds state
                oldTrace = trace state
                currentNode = nodeId state
            in processLetBindings xs $ state { nameBinds = oldBinds `Map.union` bs,
                                               trace = oldTrace ++ [(currentNode,[],"init")],
                                               nodeId = currentNode + 1
                                             }
          processLetBindings ((Let vs (Just a) rs):xs) state = -- consume rs, produce vs
            let currentBinds = nameBinds state -- [X2,X3,..]
                currentResources = originOfResources state
                currentInitialNode = nodeId state

                --resourcesNeeded = map fromJust $ (map Map.lookup rs) <*> [currentBinds] -- returns a list of pairs (r,n); r is taken from node n
                resourcesNeeded = map (fromMaybe (Atom "X",0)) $ (map Map.lookup rs) <*> [currentBinds] -- returns a list of pairs (r,n); r is taken from node n

                aggRes = map (\(x,y) -> (head x, y)) $ map unzip $ groupBy (\a b -> fst a == fst b) . sort $ resourcesNeeded
--                flattenResourcesNeeded = nub (map fst resourcesNeeded) -- list of resources needed (w/out repetitions)
--                aggRes = zip flattenResourcesNeeded $ map (fromJust . (flip Map.lookup) currentResources) flattenResourcesNeeded
                -- we now have resources aggregated: e.g. [(a,[0]), (b, [0,1])]
                
                -- IMPORTANT: because we are dealing with Celf outputs, *we know* that the resources ARE available.
                -- Hence, we do not need to compute where they can come from. We can just change the originOfResources
                -- and determine if we create an OR node.
                --fromASingleNode (t,nodes) = null $ (fromJust (Map.lookup t currentResources)) \\ nodes
--                fromASingleNode (t,nodes) = null $ (fromMaybe [] (Map.lookup t currentResources)) \\ nodes
                fromASingleNode (t,nodes) = length((fromMaybe [] (Map.lookup t currentResources))) <= length((nodes))
        
                (directLinks,orLinks) = partition fromASingleNode aggRes

                -- Before we consume the directLinks, let us keep track of the nodes we need to link from directly
                lookupInMap = (flip Map.lookup) (originOfResources state)
                flattenResourcesNeeded = nub (map fst directLinks) -- list of resources that are sources of direct links
                fromLinks = nub $ concatMap (fromMaybe [-42] . lookupInMap)  flattenResourcesNeeded
                --fromLinks = nub $ concatMap (fromJust . lookupInMap)  flattenResourcesNeeded

                state' = createAndConsumeORNodes orLinks state
                -- The trace, originOfResources, and nodeId were now updated with orNodes. We now have
                -- to create the direct links

                newState = consumeDirectLinks directLinks state'
                currentNode = nodeId newState
                currentTrace = trace newState

--                fromLinks = nub $ concatMap snd directLinks
--                flattenResourcesNeeded = nub (map fst resourcesNeeded) -- list of resources needed (w/out repetitions)
                orNodesId = [currentInitialNode..(currentNode-1)] -- if there were no OR nodes added, this list is empty
                newNode = (currentNode, fromLinks++orNodesId, a)

                -- Produce resources
                newResources = detensor $ getLeft $ fromMaybe (Atom "Z")  $ Map.lookup a actions
                newMap = foldr (\k -> Map.insertWith (++) k [currentNode]) (originOfResources newState) newResources
                newBinds = map (\(var,atom) -> (var,(atom,currentNode)) ) $ zip vs newResources -- (VAR, ATOM)


            in processLetBindings xs $  newState { originOfResources = newMap,
                                                trace = newNode:currentTrace,
                                                nodeId = currentNode + 1,
                                                nameBinds = currentBinds `Map.union` (Map.fromList newBinds)
                                              }

getLeft ((:-@:) t1 t2 _) = t2
getLeft _ = error "The program should never enter this state! Actions must be lollipops."


consumeDirectLinks [] state = state
consumeDirectLinks (o:os) state =
    let currentResources = originOfResources state
        term = fst o
        --resourceAvailableFrom = fromJust $ Map.lookup term currentResources
        -- causes problem
        resourceAvailableFrom = fromMaybe [] $ Map.lookup term currentResources

        consumeList = snd o
        -- the resource should now be available from the or node
        newOriginOfResources = Map.insert term (resourceAvailableFrom\\consumeList) currentResources
        
        newState = state { --nodeId = currentNode + 1,
                           --trace = orNode:currentTrace,
                           originOfResources = newOriginOfResources
                         }
    in consumeDirectLinks os newState



createAndConsumeORNodes [] state = state
createAndConsumeORNodes (o:os) state =
    let currentNode = nodeId state
        currentResources = originOfResources state
        currentTrace = trace state
        term = fst o
        resourceAvailableFrom = fromJust $ Map.lookup term currentResources
        orNode = (currentNode, nub resourceAvailableFrom , "OR")

        -- the resource should now be available from the or node
        numAvailable = length resourceAvailableFrom
        numConsumed = length (snd o)
        newOriginOfResources = Map.insert term (replicate (numAvailable-numConsumed) currentNode) currentResources
        
        newState = state { nodeId = currentNode + 1,
                           trace = orNode:currentTrace,
                           originOfResources = newOriginOfResources
                         }
    in createAndConsumeORNodes os newState

openHandle inp = do
    if (null inp) then return stdin
                  else do inh <- openFile inp ReadMode
                          return inh


