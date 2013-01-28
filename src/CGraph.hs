module CGraph where

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.List (sort, group, nub, partition)

import Data.Graph.Inductive.Graph (Node)
import Data.GraphViz hiding (toNode)
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as T

-- Local imports
import ProverState (Trace, ProverState(..))
import Syntax (Term(..))


-- Configuration parameters of the causality graph (Graphviz dependent)
cGraphParams = nonClusteredParams { 
    globalAttributes = [ GraphAttrs { attrs = [BgColor [X11Color Transparent]]}],
    fmtNode = cGraphLabelNodes,
    fmtEdge = const []
}

cGraphLabelNodes :: (Node,String) -> Attributes
cGraphLabelNodes (n,"OR") = [toLabel "OR", Shape BoxShape, BgColor [X11Color Gray]]
-- I assume that any description that starts with \\ is a LaTeX description
cGraphLabelNodes (n,l@('\\':_)) = [toLabel l, BgColor [X11Color Yellow], UnknownAttribute (T.pack "texmode") (T.pack "math")]
cGraphLabelNodes (n,l) = [toLabel l] -- TODO: use color to identify how these were triggered, Color [X11Color Red]]



mkCGraph :: Trace -> ([(Int,String)],[(Int,Int, String)])
mkCGraph = (id >< concat) . unzip . map (split toNode toLEdge)

removeBigBang :: ([(Int,String)],[(Int,Int, String)]) -> ([(Int,String)],[(Int,Int, String)])
removeBigBang (n,e) = (filter fstZero n, filter (\t -> fst3Zero t && snd3Zero t) e)
    where fstZero (a,b)    = a/=0
          fst3Zero (a,b,c) = a/=0
          snd3Zero (a,b,c) = b/=0

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
        getListFromMap r = (((fromMaybe []) . lookupInMap) r) ++ (((fromMaybe []) . lookupInMap) (OfCourse r)) -- search for r and !r
        resList = map (\(r,q) -> (r,q,getListFromMap r)) aggRes
    in resList

-- TODO: introduce names for the types
-- First element of pair: multiple nodes
-- Second element of the pair: resources available from only one node
partitionResourceNodeList :: [(Term,Int,[Int])] -> ([(Term,Int,[Int])],[(Term,Int,[Int])])
partitionResourceNodeList l = partition multipleNodes l
    where multipleNodes (_,_,nds) = (length . nub) nds > 1
