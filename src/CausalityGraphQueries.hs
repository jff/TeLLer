module CausalityGraphQueries where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Data.Graph.Inductive 

import Data.Maybe
import Data.List (intersect, union, findIndices, (\\))


type ActionName = String
data CGQuery = Link ActionName ActionName
             | Uno UOP CGQuery
             | Duo BOP CGQuery CGQuery
             deriving (Show, Eq)


data UOP = Not deriving (Show,Eq)
data BOP = And | Or | Iff deriving (Show, Eq)


ex0 = "link a1 a2"
ex1 = "link a1 [a2,a3]" -- todo
ex2 = "link [a1,a2] a3" -- todo
ex3 = "link [a1,a2] [a3,a4]" -- todo
ex4 = "causes a1" -- todo
ex5 = "causes [a1,a2]" -- todo
ex6 = ex0 ++ " && " ++ ex0
ex7 = ex0 ++ " && " ++ ex1
ex8 = ex0 ++ " || " ++ ex0
ex9 = ex0 ++ " || " ++ ex1


--- Parser
def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~|&="
              , opLetter = oneOf "~|=&"
              , reservedOpNames = ["~","||", "&&", "==", "link"]
              , reservedNames = []
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def


queryParser :: Parser CGQuery
queryParser = buildExpressionParser table action <?> "query"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "||" >> return (Duo Or)) AssocLeft]
        , [Infix (m_reservedOp "==" >> return (Duo Iff)) AssocLeft]
        ]

parseLink = do
    m_reservedOp "link"
    a1 <- m_identifier
    a2 <- m_identifier
    return (Link a1 a2)


action = m_parens queryParser
       <|> parseLink 


--- Queries semantics

-- Returns value + list of indices where query is satisfied
checkBooleanQuery :: CGQuery -> [Gr String String] -> (Bool,[Int])
checkBooleanQuery (Link a1 a2) graphs = 
    let allChecks   = map (linkExists a1 a2) graphs
        indicesTrue = findIndices (==True) allChecks
    in (and allChecks, indicesTrue)
checkBooleanQuery (Uno Not query) graphs = 
    let (r, is) = checkBooleanQuery query graphs
        numGraphs = length graphs
    in (not r, [0..numGraphs-1] \\ is)
checkBooleanQuery (Duo And query1 query2) graphs = 
    let (r1, is1) = checkBooleanQuery query1 graphs
        (r2, is2) =checkBooleanQuery query2 graphs
    in (r1 && r2, is1 `intersect` is2)
checkBooleanQuery (Duo Or query1 query2) graphs = 
    let (r1, is1) = checkBooleanQuery query1 graphs
        (r2, is2) = checkBooleanQuery query2 graphs
        unionIndices = is1 `union` is2
        numGraphs = length graphs
    in (numGraphs == length unionIndices, unionIndices)

checkBooleanQuery _ _ = error "[checkBooleanQuery] NOT IMPLEMENTED!"


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




