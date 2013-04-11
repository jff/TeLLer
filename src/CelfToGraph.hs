{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

-- Import GraphViz to generate graphs
import Data.Graph.Inductive (Gr, mkGraph)
import Data.GraphViz (runGraphviz, graphToDot,GraphvizOutput(..), isGraphvizInstalled)

import System.Console.CmdArgs
import System.IO (hPutStrLn, stderr, stdin, openFile, IOMode(..), hClose, hGetContents)
--import Control.Monad (when)
import Control.Monad.State -- (lift, evalStateT, get, gets, put, modify, when)
import qualified Data.Map as Map -- hiding (null, foldr)
import Control.Applicative
import Data.Maybe (fromJust, fromMaybe)
import Data.List (nub, sort, groupBy, (\\), partition)

-- Local imports
import Syntax
import Parser (tts')
import Term (detensor)

-- Local imports
import CGraph


data Args = Args {
      inputFile  :: FilePath,
      outputFile :: FilePath
    } deriving (Eq, Show, Typeable, Data)

defaultArgs :: Args
defaultArgs = Args {
               outputFile = "celfGraph.png"
               &= typFile
               &= groupname "Output file"
               &= help "File where the graph is saved to"

               ,inputFile = def
               &= args
               &= typ "FILE"
              } &= verbosity
                &= summary ("CelfToGraph: extraction of causality graphs from Celf traces" )


{--
validateArgs :: Args -> IO ()
validateArgs Args{..} = do
  dirExists <- doesDirectoryExist output
  let p !? what | p         = Just what
                | otherwise = Nothing
      infix 1 !?
      problems = catMaybes [
        dirExists    !? "The output directory must be a non-existing directory",
        (null files) !? "At least one data source has to be specified"
       ]
  forM_ problems $ hPutStrLn stderr . ("Error: " ++)
  unless (null problems) $ exitWith (ExitFailure 1)
--}


data CState = State {
    nameBinds         :: Map.Map String (Term,Int), -- the int is the nodeid
    originOfResources :: Map.Map Term [Int],
    trace :: [(Int, [Int], String)],
    nodeId :: Int
} deriving Show

initialState = State {
    nameBinds = Map.empty,
    originOfResources = Map.empty,
    trace = [],
    nodeId = 0
}

type CStateIO a = StateT CState IO a

main :: IO ()
main = do
  as@Args{..} <- cmdArgs $ defaultArgs &= program "CelfToGraph"
  --validateArgs as
  putStrLn $ "Output: " ++ outputFile
  --inh <- openHandle inputFile
  --inpStr <- hGetContents inh
  --putStrLn inpStr
  -- hClose inh
  evalStateT (test y) initialState
  putStrLn "Goodbye."

test x = do
--    names <- gets nameBinds
--    lift $ putStrLn $ show names
    state <- get
    lift $ putStrLn $ show state
    modify (addInitialResources x)
    state <- get
    lift $ putStrLn $ show state
--    o <- gets originOfResources
 --   lift $ putStrLn $ show o
    state <- get
    lift $ putStrLn $ show state
    modify (processLambda x)
    state <- get
    lift $ putStrLn $ show state
    t <- gets trace
    let (nds,eds) = mkCGraph t
    let cgr = mkGraph nds eds :: Gr String String
    lift $ runGraphviz (graphToDot cGraphParams cgr) Pdf "jff.pdf"
 
--    modify (f names)

-- TODO: parameterize the number of nodes
addInitialResources :: CelfOutput -> CState -> CState
addInitialResources (Celf init _ _) state = 
    let rs = concatMap detensor init
        newMap = foldr (\k -> Map.insertWith (++) k [0]) (originOfResources state) rs
    in
    state { originOfResources = newMap }


f :: Map.Map String Term -> CState -> CState
f names state = undefined -- state { nameBinds = fromList [("A",] }

type Var = String
type RuleName = String
data CelfOutput = Celf [Term] (Map.Map String Term) Solution deriving (Show)
data Solution   = Lambda Var [LetBinding] deriving (Show)
data LetBinding = Let [Var] (Maybe RuleName) [Var] deriving (Show)

-- generated manually
x :: CelfOutput
x = Celf (tts' "a*b")
         (Map.fromList [("a1",head $ tts' "a-@b*b"),("a2",head $ tts' "b*b-@c")])
         (Lambda "X1" [ Let ["X2","X3"] Nothing     ["X1"],
                        Let ["X4","X5"] (Just "a1") ["X2"],
                        Let ["X6"]      (Just "a2") ["X5","X3"]
                      ])


y :: CelfOutput
y =  Celf (tts' "k * l * p")
          (Map.fromList [("o1", head $ tts' "k-@m"),
                         ("o2", head $ tts' "l-@m"),
                         ("a1", head $ tts' "l-@d"),
                         ("a2", head $ tts' "p-@m*m"),
                         ("a3", head $ tts' "m*m-@f")
                        ])
          (Lambda "X1" [
                            Let ["X2","X3","X4"] Nothing ["X1"],
                            Let ["X5","X6"] (Just "a2") ["X4"],
                            Let ["X7"] (Just "o2") ["X3"],
                            Let ["X8"] (Just "a3") ["X5","X6"],
                            Let ["X9"] (Just "o1") ["X2"],
                           Let ["X10"] (Just "a3") ["X9","X7"]
                       ])
--o1: k -o {@m}.
--o2: l -o {@m}.
--a1: m -o {@d}.
--a2: p -o {@m * @m}.
--a3: (m * m) -o {@f}.
--init: Type = {k * (l * p)}.
--Solution: \@X1. {
--    let {[X2, [X3, X4]]} = X1 in 
--    let {[@X5, @X6]} = a2 X4 in 
--    let {@X7} = o2 X3 in 
--    let {@X8} = a3 [X5, X6] in 
--    let {@X9} = o1 X2 in 
--    let {@X10} = a3 [X9, X7] in X10}
--

processLambda :: CelfOutput -> CState -> CState
processLambda (Celf init actions (Lambda _ ls)) state = processLetBindings ls state
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
                fromLinks = nub $ concatMap (fromJust . lookupInMap)  flattenResourcesNeeded

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

-- | 'printGraph' creates a JPEG image of the causality graph and saves it to the file name given
--   as argument.
--printGraph :: FilePath -> ProverStateIO ()
{--
printGraph filename = do
    ginstalled <- isGraphvizInstalled
    when (not ginstalled) $ do
        hPutStrLn stderr "Graphviz is not installed. Please install it and try again."
    when (ginstalled) $ do
        trace <- gets actionTrace
        let (nds,eds) = mkCGraph trace
        let cgr = mkGraph nds eds :: Gr String String
        runGraphviz (graphToDot cGraphParams cgr) Pdf filename
        runGraphviz (graphToDot cGraphParams cgr) DotOutput (filename++".dot")
--}




