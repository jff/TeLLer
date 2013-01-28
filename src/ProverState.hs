-- | The module 'ProverState' defines the datatype used as TeLLer's state together with
--   functions that manipulate the state. It is divided in three main sections: 
--
--      0. The first section defines all the relevant data types
--
--      1. The second section defines pure functions that receive the state
--         as argument and yield a new state.
--
--      2. The third section defines monadic functions that change the state.
--         TODO: this needs a major cleanup!

module ProverState where

import Control.Monad.State
import Data.List ((\\))
import qualified Data.Map as Map

-- Local imports
import Syntax (Term)
import Term (linearizeTensorProducts, listEnabledActionsBy, isAtomicResource)
import Parser (parse,term,tts') 
import Printer (showTerms) -- used to show a state as a string
import RewriteRules (simplify)
import UserIO (tellerError)



-----------------------------------------------------------------------------------------------
-- 0. Data types
-----------------------------------------------------------------------------------------------

-- | Granularity is the measure that we use when focusing. At the moment, we define iWe define its type as 'Int'.
type Granularity = Int 

-- | Environment is a multiset of terms (here, represented as a sequence)
type Environment = [Term]

-- | A trace is a list of triples (n,ln,a), meaning that action identified by 'a' is associated with
--   a node n of the causality graph. The list 'ln' is the list of nodes that provide resources that action
--   'a' can use. Actions are identified as Strings, because that's how they will be shown in the causality graph.
type Trace = [(Int,[Int],String)]

-- | ProverState is the datatype that models TeLLer's state.
data ProverState = ProverState 
    { env :: Environment,       -- ^ The field 'env' is the sequence of terms that is used in the focused reductions.
      unfocused :: [Term], -- ^ The field 'unfocused' is a sequence of actions that are unfocused and will not be used 
                           --   in the focused reductions.
      actionTrace :: Trace, -- ^ The field 'actionTrace' represents the causality trace. 
      originOfResources :: Map.Map Term [Int],  -- ^ The map 'originOfResources' maps resources to the list of nodes where they are available.
                                                --   The number of the node is listed as many times as there are resources.
                                                --   For example, if the action associated with node 2 produces two Bs, then the map
                                                --   would associate B with [2,2].
      _cGraphNode :: Int, -- ^ Internal identifier used for the causality graph nodes
      
      granularity :: Granularity, -- ^ The field 'granularity' defines the number of focused reductions to be performed.
      focusedReductions :: Int,   -- ^ The field 'focusedReductions' is a counter for the number of focused reductions performed so far.
      totalReductions :: Int,     -- ^ The field 'totalReductions' is a counter for the total number of reductions performed so far.
      debugMode :: Bool           -- ^ The field 'debugMode' defines whether TeLLer runs in debug mode.
    }

-- | The initial state.
initialState = ProverState { env = [], 
                             unfocused = [], 
                             actionTrace = [(0,[],"\\emptyset_0")], 
                             originOfResources = Map.empty,
                             _cGraphNode = 0,
                             granularity = 2, 
                             focusedReductions = 0, 
                             totalReductions = 0, 
                             debugMode = False
                            }

-- | The Monad Transformer that stacks our state with IO
type ProverStateIO a = StateT ProverState IO a

-----------------------------------------------------------------------------------------------
--- 1. Pure functions
-----------------------------------------------------------------------------------------------
changeEnvTo :: [Term] -> ProverState -> ProverState
changeEnvTo newEnv state = state { env = linearizeTensorProducts newEnv } 

addToEnv :: [Term] -> ProverState -> ProverState
addToEnv resources state = 
    let reductions = totalReductions state
        nodeNumber = if (reductions>0) then (_cGraphNode state) + 1 else 0
        atoms = filter isAtomicResource resources
        newMap = foldr (\k -> Map.insertWith (++) k [nodeNumber]) (originOfResources state) atoms
    in if (reductions>0)
       then state {
                env = (env state)++(linearizeTensorProducts resources),
                _cGraphNode = nodeNumber,
                originOfResources = newMap,
                actionTrace = (actionTrace state) ++ [(nodeNumber,[],"\\emptyset_"++(show nodeNumber))]
            } 
        else
            state { env = (env state)++(linearizeTensorProducts resources) }

-- TODO: removeFromEnv is quadratic!; can this improve?
removeFromEnv :: [Term] -> ProverState -> ProverState
removeFromEnv resources state = state {env = (env state) \\ (linearizeTensorProducts resources) }

simplifyEnv :: ProverState -> ProverState
simplifyEnv state = state { env = map simplify (env state) }

moveUnfocusedToEnv :: ProverState -> ProverState
moveUnfocusedToEnv state = state { env = (env state) ++ (unfocused state),  unfocused = [] }


changeDebugMode :: ProverState -> ProverState
changeDebugMode state = state {debugMode = not (debugMode state)} 

changeGranularityTo :: Int -> ProverState -> ProverState
changeGranularityTo n state = state { granularity = n }

setFocusedReductionsTo :: Int -> ProverState -> ProverState
setFocusedReductionsTo n state = state { focusedReductions = n }

setTotalReductionsTo :: Int -> ProverState -> ProverState
setTotalReductionsTo n state = state { totalReductions = n }

setCountersToZero :: ProverState -> ProverState
setCountersToZero state = state { totalReductions = 0, focusedReductions = 0 }

incrementGraphNodeCount :: ProverState -> ProverState
incrementGraphNodeCount state = state { _cGraphNode = (_cGraphNode state) + 1}

-- Causality Graph
addActionToTrace :: (Int,[Int],String) -> ProverState -> ProverState
addActionToTrace action state = state { actionTrace = (actionTrace state) ++ [action] }

-- non-OR nodes: consume the first k (where k is the second element in the triple) from the map
-- If l==[], then the map is unchanged
changeMapNonOR :: [(Term, Int, [Int])] -> ProverState -> ProverState
changeMapNonOR l state =
    let originRes = originOfResources state
        newMap = foldr (\(k,qty,_) -> Map.adjust (drop qty) k) originRes l
    in state {originOfResources = newMap}

-- OR nodes: 
-- If l==[], then the map is unchanged
changeMapOR :: [(Term, Int, [Int])] -> Int -> ProverState -> ProverState
changeMapOR l orNode state =
    let originRes = originOfResources state
        newMap = foldr (\(k,qty,nds) -> Map.insert k (replicate ((length nds) - qty) orNode)) originRes l
    in state {originOfResources = newMap}


showState :: ProverState -> String
showState state = "Current focused resources: \n"   ++ showTerms (env state) ++ "\n" ++
                  "Current unfocused resources: \n" ++ showTerms (unfocused state) ++ "\n"


-- TODO: Not a good idea to have this here and in addToEnv...
initOriginMapWithAtoms :: ProverState -> ProverState
initOriginMapWithAtoms state =
    let ctx = linearizeTensorProducts $ env state
        atoms = filter isAtomicResource ctx
        omap = originOfResources state
        newMap = foldr (\k -> Map.insertWith (++) k [0]) omap atoms
    in state {originOfResources = newMap}


-----------------------------------------------------------------------------------------------
-- 2. Non pure functions
-----------------------------------------------------------------------------------------------

-- | 'changeEnvWith' receives a function 'f' and a string 's' describing resources. If it
--   successfully parses 's', then it changes the state using function 'f'.
changeEnvWith :: ([Term] -> ProverState -> ProverState) -> String -> ProverStateIO ()
changeEnvWith f t = do
    case parse term "<interactive>" t of
     Left err -> lift $ tellerError "ERROR: Parsing error. Please try again." 
     Right r  -> modify (f r) 



-- TODO: define increaseIntField and send as argument the field? Is it possible?
-- returns the new value
increaseTotalReductions :: Int -> ProverStateIO Int
increaseTotalReductions n = do
    state <- get
    let newValue = 1 + totalReductions state
    put $ state { totalReductions = newValue }
    return newValue

resetAllCounters :: ProverStateIO ProverState
resetAllCounters = do 
    state <- get
    let newState = state { totalReductions = 0, focusedReductions = 0 }
    put newState
    return newState

-- returns the new value
increaseFocusedReductions :: Int -> ProverStateIO Int
increaseFocusedReductions n = do
    state <- get
    let newValue = 1 + focusedReductions state
    put $ state { focusedReductions = newValue }
    return newValue

-- returns the number of focused reductions
increaseNumberOfReductionsBy :: Int -> ProverStateIO Int
increaseNumberOfReductionsBy n = do
    increaseTotalReductions n 
    increaseFocusedReductions n 
                                    
-- TODO: remove the following, as it is equivalent to gets totalReductions
getNumberTotalReductions :: ProverStateIO Int
getNumberTotalReductions = do
    state <- get
    return $ totalReductions (state)

unfocus :: Term -> ProverStateIO ()
unfocus t = do
    state <- get
    let gran = granularity state
    let s = env state
    when (gran>0) $ do
        let newEnv = s \\ [t]
        put (state {env = newEnv, unfocused = t:(unfocused state)})


focusActionsEnabledBy :: Term  -> ProverStateIO [Term]
focusActionsEnabledBy t = do
    let newResources = linearizeTensorProducts [t]
    state <- get
    let unFocusedActions = unfocused state
    let actionsToFocus = listEnabledActionsBy newResources unFocusedActions --filter (isEnabledAction [t]) unFocusedActions
--    when (debugMode state) $ lift $ flushStrLn $ "[DEBUG] Focusing actions " ++ (show actionsToFocus)
    put $ state { env = (env state) ++ actionsToFocus, unfocused = unFocusedActions \\ actionsToFocus }
    return actionsToFocus
    -- move from unfocus to env


findFixpointStateIO :: Eq a => (a -> ProverStateIO a) -> a -> ProverStateIO a
findFixpointStateIO f x =
  do x' <- f x
     case x' == x of
       True  -> return x'
       False -> findFixpointStateIO f x'


-- Utilities:

createEnvFromFile :: FilePath -> IO ProverState
createEnvFromFile fileName = do
    fileContents <- readFile fileName 
    return (changeEnvTo (tts' fileContents) initialState)
