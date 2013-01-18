module ProverState where

import Syntax
import Control.Monad.State
import Data.List ((\\))
import Term

import Util  --TODO: For debug only, remove when finished

type Granularity = Int 
data ProverState = ProverState 
    { env :: [Term],       -- focused environment
      unfocused :: [Term], -- unfocused environment
      granularity :: Granularity,
      focusedReductions :: Int,
      totalReductions :: Int,
      debugMode :: Bool
    }
initialState = ProverState { env = [], 
                             unfocused = [], 
                             granularity = 2, 
                             focusedReductions = 0, 
                             totalReductions = 0, 
                             debugMode = False 
                            }

type ProverStateIO a = StateT ProverState IO a

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
    when (debugMode state) $ lift $ flushStrLn $ "[DEBUG] Focusing actions " ++ (show actionsToFocus)
    put $ state { env = (env state) ++ actionsToFocus, unfocused = unFocusedActions \\ actionsToFocus }
    return actionsToFocus
    -- move from unfocus to env
