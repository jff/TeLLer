module CGraph where

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.List (sort, group, nub)

import Data.Graph.Inductive.Graph (Node)
import Data.GraphViz (nonClusteredParams,toLabel,fmtNode, globalAttributes, fmtEdge, Attributes(..))

-- Local imports
import ProverState (Trace, ProverState(..))
import Syntax (Term)


-- Configuration parameters of the causality graph (Graphviz dependent)
cGraphParams = nonClusteredParams { 
    globalAttributes = [],
    fmtNode = cGraphLabelNodes,
    fmtEdge = const []
}

cGraphLabelNodes :: (Node,String) -> Attributes
cGraphLabelNodes (n,l) = [toLabel l]




mkCGraph :: Trace -> ([(Int,String)],[(Int,Int, String)])
mkCGraph = (id >< concat) . unzip . map (split toNode toLEdge)

toNode :: (Int,[Int],String) -> (Int, String)
toNode (r,l,a) = (r,a)

toLEdge :: (Int,[Int],String) -> [(Int, Int, String)]
toLEdge (r,l,a) = [(from,r,"") | from <- l]

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g a = (f a, g a)

(><) :: (a->c) -> (b->d) -> (a,b) -> (c,d)
(f >< g) (a,b) = (f a, g b)


-- Operations on the state
getResourceNodeList :: ProverState -> [Term] -> [(Term,Int,[Int])]
getResourceNodeList state needs = 
    let nodeRes = originOfResources state
        -- aggRes agglomerates resources. Example: [A,B,C,B] is transformed into [(A,1), (B,2), (C,1)]
        aggRes = map (\l -> (head l, length l)) . group . sort $ needs
        lookupInMap = (flip Map.lookup) nodeRes
        getListFromMap r = ((fromMaybe []) . lookupInMap) r
        resList = map (\(r,q) -> (r,q,getListFromMap r)) aggRes
    in resList

-- TODO: introduce names for the types
partitionResourceNodeList :: [(Term,Int,[Int])] -> ([(Term,Int,[Int])],[(Term,Int,[Int])])
partitionResourceNodeList l = break multipleNodes l
    where multipleNodes (_,_,nds) = (length . nub) nds > 1
