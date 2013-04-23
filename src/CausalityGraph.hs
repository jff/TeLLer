------------------------------------------------------------------
-- This is the new module used to generate causality graphs. It
-- replaces CGraph.
------------------------------------------------------------------
module CausalityGraph where

import Data.Graph.Inductive hiding ((><)) -- (Node)
import Data.GraphViz hiding (toNode)
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as T

import Data.List (nub, (\\), sort, group, partition, intersect)
import qualified Data.Map as Map
import Data.Maybe

-- Local imports
import Syntax (Term(..))
import Term (detensor)
import Printer (showTerm)

-- Debug
import Parser (tts')

------------------------------------------------------------------
-- Data types.
------------------------------------------------------------------
type Environment = [Term] -- Environments are lists of terms, normally atomic
type ActionTrace = [Term] -- Action traces are lists of actions
type Actions = [Term] -- List of actions available

type NodeId = Int
type GraphList = [(NodeId, [NodeId], String)] -- Graphs as lists. Each triple (id, [from], name) represents a node with id 'id', links from
                                        -- all the nodes with ids given by '[from]', and with name 'name'
type ResourcesLocation = Map.Map Term [NodeId]

data Traces = Trace Environment [ActionTrace]
    deriving (Show, Eq)

------------------------------------------------------------------
-- Graph generation functions. `graphsFromTraces` is the main
-- function.
------------------------------------------------------------------

graphsFromTraces :: Traces -> [Gr String String]
graphsFromTraces ts = -- creates one graph for each ts
    let graphs = createGraphsLists ts -- creates [[(Int,[Int],String)]]
        cgs = map mkCGraph graphs     -- mkCGraph creates pair with nodes and edges ([(Int,String)],[(Int,Int, String)])
    in map (uncurry mkGraph) cgs :: [Gr String String]

createGraphsLists :: Traces -> [GraphList]
createGraphsLists (Trace initEnv ts) = map (createGraph initEnv) ts

createGraph :: Environment -> ActionTrace -> GraphList
createGraph initEnv t = executeActions initEnv t initialRLoc 1 initialGraphList
    where initialRLoc = addResourcesLocations initEnv 0 Map.empty
          initialGraphList = [(0,[],"init")]

executeActions :: Environment -> ActionTrace -> ResourcesLocation -> NodeId -> GraphList -> GraphList
executeActions _ [] _ _ currentGraph = currentGraph
executeActions initEnv (action:as) currentRLoc currentNode currentGraph = 
    let resourcesNeeded = linearizeTensorProducts $ [getLeftLolli action]
        -- |resourcesNeeded| is a list of all the resources needed to execute |action|
        -- We now aggregate these resources in |resourcesAgg|. For example, if |resourcesNeeded|
        -- is [A,A,!B,C,C,C], |resourcesAgg|=[(A,2),(!B,1),(C,3)] 
        resourcesAgg = map (\l -> (head l, length l)) . group . sort $ resourcesNeeded

        -- We now create a list of triples similar to |resourcesAgg|, but containing the locations (node ids) of the needed resources.
        -- The name of the list is |resourcesAggLoc|.
        resourcesAggLoc = map (\(r,q) -> (r,q,getResLocListFromMap r)) resourcesAgg
        -- |getResLocListFromMap r| gets the locations of r and !r
        getResLocListFromMap r = (((fromMaybe []) . lookupInMap) r) ++ (((fromMaybe []) . lookupInMap) (OfCourse r)) 
        lookupInMap = (flip Map.lookup) currentRLoc

        (orLinks, directLinks) = partitionResourceNodeList resourcesAggLoc
        directLinksNodeList = nub $ concatMap (\(_,_,c)->c) directLinks

        nodeLabel = getActionName action

        (newNodeId, newGraph) = 
            if (orLinks==[]) 
            then (currentNode+1, (currentNode, directLinksNodeList, nodeLabel):currentGraph)
            else 
                let multipleNodesNonDup = nub (concatMap (\(_,_,c)->c) orLinks)
                    orNode = (currentNode, multipleNodesNonDup, "OR")
                    newNodeId = currentNode + 1
                    -- If this action also depends of resources that originate from a single node, we need
                    -- to establish those connections in the causality graph
                in if (directLinksNodeList == []) 
                   then (newNodeId+1, (newNodeId,[newNodeId-1],nodeLabel): orNode: currentGraph)
                   else (newNodeId+1, (newNodeId,(newNodeId-1):directLinksNodeList,nodeLabel): orNode: currentGraph)
 
        conservedRes = conservedResources action -- new
        remExcludeConserved = filter (\(t,_,_) ->(not $ t `elem` conservedRes)) -- new 
        rL1 = changeMapNonOR (remExcludeConserved directLinks) currentRLoc -- new
        rL2 = changeMapOR (remExcludeConserved orLinks) (newNodeId-2) rL1 -- new
        --rL1 = changeMapNonOR directLinks currentRLoc
        --rL2 = changeMapOR orLinks (newNodeId-2) rL1

        introduces = linearizeTensorProducts [(getRightLolli action)]
        introExcludeConserved = introduces \\ conservedRes -- new
        newRLoc = foldr (\k -> Map.insertWith (++) k [newNodeId-1]) rL2 introExcludeConserved -- new
        --newRLoc = foldr (\k -> Map.insertWith (++) k [newNodeId-1]) rL2 introduces
    in executeActions initEnv as newRLoc newNodeId newGraph

conservedResources ((:-@:) t1 t2 _) = 
    let left = linearizeTensorProducts [t1]
        right = linearizeTensorProducts [t2]
    in intersect left right
conservedResources t = error $ "[conservedResources] In the traces given, one of the actions is not a lollipop! Please fix that and try again." 

-- | `changeMapNonOR` updates the resources location map with the new nodes information.
--    If the first argument is [], then the map is unchanged.
changeMapNonOR :: [(Term, Int, [Int])] -> ResourcesLocation -> ResourcesLocation
changeMapNonOR l resourcesLocation = foldr (\(k,qty,_) -> Map.adjust (drop qty) k) resourcesLocation l

-- | `changeMapOR` updates the resources location map with the new OR node information.
--    If the first argument is [], then the map is unchanged.
changeMapOR :: [(Term, Int, [Int])] -> Int -> ResourcesLocation -> ResourcesLocation
changeMapOR l orNode resourcesLocation = foldr (\(k,qty,nds) -> Map.insert k (replicate ((length nds) - qty) orNode)) resourcesLocation l

partitionResourceNodeList :: [(Term,Int,[Int])] -> ([(Term,Int,[Int])],[(Term,Int,[Int])])
partitionResourceNodeList l = partition multipleNodes l
    where multipleNodes (_,qty,nds) = (length . nub) nds > 1 && length nds > qty

addResourcesLocations :: [Term] -> NodeId -> ResourcesLocation -> ResourcesLocation
addResourcesLocations resources currentNode currentRLoc =
    let dResources = linearizeTensorProducts resources
        newRLoc = foldr (\k -> Map.insertWith (++) k [currentNode]) currentRLoc dResources
    in newRLoc

------------------------------------------------------------------
-- Configuration parameters of the causality graph. These can be
-- changed to adjust how the graph looks like.
-- This is Graphviz-dependent.
------------------------------------------------------------------

cGraphParams = nonClusteredParams { 
    globalAttributes = [ GraphAttrs { attrs = [BgColor [X11Color Transparent]]}],
    fmtNode = cGraphLabelNodes,
    fmtEdge = const []
}

cGraphLabelNodes :: (Node,String) -> Attributes
cGraphLabelNodes (n,'_':'l':'_':l) = [toLabel l, BgColor [X11Color Gray], style dashed, style filled]
cGraphLabelNodes (n,_:_:_:"OR") = [toLabel "OR", Shape BoxShape, BgColor [X11Color Gray]]
-- I assume that any description that starts with \\ is a LaTeX description
cGraphLabelNodes (n,(_:_:_:l@('\\':_))) = [toLabel l, BgColor [X11Color Yellow], UnknownAttribute (T.pack "texmode") (T.pack "math")]
cGraphLabelNodes (n,l) = [toLabel (drop 3 l)] -- TODO: use color to identify how these were triggered, Color [X11Color Red]]


------------------------------------------------------------------
-- Queries on graphs. TODO: tidy up this section.
------------------------------------------------------------------
-- find the id of a node from its label
--findNodeId :: (Eq a) => Gr a b -> a -> Node
findNodeId g l = 
    let swap (x,y) = (y,x)
        lassoc = map swap (labNodes g)
        maybeId = lookup l lassoc
    in  fromMaybe (-1) maybeId --fromMaybe -1 $ (lookup l) (map swap (labNodes g))

-- is there a link from x to y
-- TODO: confirm that there is no point using x of the form _l_STRING
--linkExists :: Gr String b -> String -> String -> Bool
linkExists x y g = 
    let nx = findNodeId g ("_o_"++x)
        nyl = findNodeId g ("_l_"++y)
        nyo = findNodeId g ("_o_"++y)
    in if (nx>(-1)) then
        let reachable = dfs [nx] g
        in (nyl `elem` reachable) || (nyo `elem` reachable)
       else False



------------------------------------------------------------------
-- Auxiliary functions.
------------------------------------------------------------------

----------------------------------
-- Functions on traces
----------------------------------

showActionTraces :: Traces -> [String]
showActionTraces (Trace _ at) = map (seqActions . map getActionName) at
    where seqActions = foldr1  (\a b -> (a++";\n"++b))

----------------------------------
-- Functions on graphs
----------------------------------

-- | `mkCGraph` converts a |GraphList| into a pair of nodes and edges.
mkCGraph :: GraphList -> ([(Int,String)],[(Int,Int, String)])
mkCGraph t = (id >< concat) . unzip . map (split (toNode t) toLEdge) $ t

toNode :: GraphList -> (Int,[Int],String) -> (Int, String)
toNode trace (r,l,a) = 
    let allNodes  = nub $ map (\(f,s,t) -> f) trace
        fromNodes = nub $ concatMap (\(f,s,t) -> s) trace -- nodes which have an outgoing arrow
        leaves    = allNodes\\fromNodes
    in if(r `elem` leaves) then (r,"_l_"++a) -- origin action
                           else (r,"_o_"++a)  -- leave action

toLEdge :: (Int,[Int],String) -> [(Int, Int, String)]
toLEdge (r,l,a) = [(from,r,"") | from <- l]


----------------------------------
-- Functions on terms
----------------------------------

getLeftLolli ((:-@:) t1 t2 _) = t1
getLeftLolli t = error $ "[getLeftLolli] In the traces given, one of the actions is not a lollipop! Please fix that and try again." 

getRightLolli ((:-@:) t1 t2 _) = t2
getRightLolli _ = error $ "[getRightLolli] In the traces given, one of the actions is not a lollipop! Please fix that and try again."

getActionName (a@((:-@:) t1 t2 Nothing)) = showTerm a
getActionName ((:-@:) t1 t2 (Just s)) = s
getActionName _ = error $ "[getActioName] In the traces given, one of the actions is not a lollipop! Please fix that and try again."

linearizeTensorProducts :: [Term] -> [Term]
linearizeTensorProducts = concatMap detensor 


----------------------------------
-- Pointfree combinators
----------------------------------

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g a = (f a, g a)

(><) :: (a->c) -> (b->d) -> (a,b) -> (c,d)
(f >< g) (a,b) = (f a, g b)


------------------------------------------------------------------
-- Some corner cases useful for testing. 
-- TODO: Move this somewhere else and use Quickcheck.
------------------------------------------------------------------
data MyTest = Test { input  :: Traces,
                     output :: [GraphList],
                     description :: String }

emptyTrace0 = Trace [] []
emptyTrace1 = Trace (tts' "Irrelevant Environment") []
emptyGraph = []
t0 = Test emptyTrace0 emptyGraph "Empty trace returns empty graph (0)"
t1 = Test emptyTrace1 emptyGraph "Empty trace returns empty graph (1)"

ex0 = Trace (tts' "Z B")
            [tts' "Z-@!A B-@A A-@B A-@C"]
cg0 = [[(6,[5],"A  -@  C"),(5,[3,1],"OR"),(4,[3],"A  -@  B"),(3,[2,1],"OR"),(2,[0],"B  -@  A"),(1,[0],"Z  -@  !A"),(0,[],"init")]]
t2 = Test ex0 cg0 "Persistent resources (0)"

ex1 = Trace (tts' "!A B")
            [tts' "B-@A A-@B A-@C"]
cg1 = [[(5,[4],"A  -@  C"),(4,[2,0],"OR"),(3,[2],"A  -@  B"),(2,[1,0],"OR"),(1,[0],"B  -@  A"),(0,[],"init")]]
t3 = Test ex1 cg1 "Persistent resources (1)"

ex2 = Trace (tts' "A C")
            [tts' "A-@B*B C-@B B*B*B-@D"]
cg2 = [[(3,[2,1],"B*B*B  -@  D"),(2,[0],"C  -@  B"),(1,[0],"A  -@  B*B"),(0,[],"init")]]
t4 = Test ex2 cg2 "OR node should not be created (0)"

ex3 = Trace (tts' "Z") [tts' "Z-@A A-@A*B A-@C"]
cg3 = [[(3,[1],"A  -@  C"),(2,[1],"A  -@  A*B"),(1,[0],"Z  -@  A"),(0,[],"init")]]
t5 = Test ex3 cg3 "Resources are conserved (0)"

ex4 = Trace (tts' "Z") [tts' "Z-@A A-@A*A*B A-@C"]
cg4 = [[(4,[3],"A  -@  C"),(3,[2,1],"OR"),(2,[1],"A  -@  A*A*B"),(1,[0],"Z  -@  A"),(0,[],"init")]]
t6 = Test ex4 cg4 "Resources are conserved (1)"

tests = [
              t0
            , t1 
            , t2 
            , t3
            , t4
            , t5
            , t6
        ]



-- | `writeToDir t d` writes to directory d the graphs generated from t
writeToDir ts dirName = do
    let graphs = graphsFromTraces ts
    let createPDF filename g = runGraphviz (graphToDot cGraphParams g) Pdf filename
    let ioCommands = zipWith ($) [createPDF (dirName++"/"++((show n)++".pdf")) | n<-[0..]] graphs
    sequence_ ioCommands
    putStrLn $ "Done. Number of graphs generated: " ++ show (length graphs)

-----------------------------
-- Basic automated testing
-----------------------------

test = testAll tests 0
    where testAll [] n = "[OK] All tests passed (" ++ show n ++ " tests in total)."
          testAll (t:ts) n | createGraphsLists (input t) == (output t) = testAll ts (n+1)
                           | otherwise                                 = "[ERROR] Test failed: " ++ description t

------------------------------------------------------------------
-- End of testing functions.
------------------------------------------------------------------


