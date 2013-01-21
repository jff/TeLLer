-- | The module 'ProverState' defines the datatype used as TeLLer's state together with
--   functions that manipulate the state. It is divided in two main sections: 
--
--      1. The first section defines pure functions that receive the state
--         as argument and yield a new state.
--
--      2. The second section defines monadic functions that change the state.

module ProverState where

import Control.Monad.State
import Data.List ((\\))

-- Local imports
import Syntax (Term)
import Term (linearizeTensorProducts, listEnabledActionsBy)
import Parser (tts')       -- used to load state from file
import Printer (showTerms) -- used to show a state as a string
import RewriteRules (simplify)


-- | Granularity is the measure that we use when focusing. We define its type as 'Int'.
type Granularity = Int 

-- | Environment is a multiset of terms (here, represented as a sequence)
type Environment = [Term]

-- | ProverState is the datatype that models TeLLer's state.
data ProverState = ProverState 
    { env :: Environment,       -- ^ The field 'env' is the sequence of terms that is used in the focused reductions.
      unfocused :: [Term], -- ^ The field 'unfocused' is a sequence of actions that are unfocused and will not be used 
                           --   in the focused reductions.
      granularity :: Granularity, -- ^ The field 'granularity' defines the number of focused reductions to be performed.
      focusedReductions :: Int,   -- ^ The field 'focusedReductions' is a counter for the number of focused reductions performed so far.
      totalReductions :: Int,     -- ^ The field 'totalReductions' is a counter for the total number of reductions performed so far.
      debugMode :: Bool           -- ^ The field 'debugMode' defines whether TeLLer runs in debug mode.
    }

-- | The initial state.
initialState = ProverState { env = [], 
                             unfocused = [], 
                             granularity = 2, 
                             focusedReductions = 0, 
                             totalReductions = 0, 
                             debugMode = False
                            }

-- | The Monad Transformer that stacks our state with IO
type ProverStateIO a = StateT ProverState IO a


--- Pure functions
changeEnvTo :: [Term] -> ProverState -> ProverState
changeEnvTo newEnv state = state { env = linearizeTensorProducts newEnv } 

addToEnv :: [Term] -> ProverState -> ProverState
addToEnv resources state = state {env = (env state)++(linearizeTensorProducts resources)} 

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

setFocusedReductionsToZero :: ProverState -> ProverState
setFocusedReductionsToZero state = state { focusedReductions = 0 }

setTotalReductionsToZero :: ProverState -> ProverState
setTotalReductionsToZero state = state { totalReductions = 0 }

setCountersToZero :: ProverState -> ProverState
setCountersToZero state = state { totalReductions = 0, focusedReductions = 0 }

showState :: ProverState -> String
showState state = "Current focused resources: \n"   ++ showTerms (env state) ++ "\n" ++
                  "Current unfocused resources: \n" ++ showTerms (unfocused state) ++ "\n"





--- Non pure functions
t = withStateT changeDebugMode 
x :: ProverStateIO ()
x = do
    state <- get
    lift $ print (debugMode state)
    return ()

changeStateWith f = withStateT f get >>= put


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
