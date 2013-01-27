module Reductions where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State

-- Local imports
import ReductionRules (StateReduction,
                       reduceLollyStateIO,
                       reduceWithStateIO,
                       reducePlusStateIO,
                       reduceOneStateIO)
import Util
import Syntax
import Printer
import ProverState
import Term
import UserIO

startTeLLer :: ProverStateIO ()
startTeLLer = do
    -- First, we apply apply some simplification rules to the environment (these are defined in 'RewriteRules')
    modify simplifyEnv

    -- We also initialize the map modelling the origin of resources
    -- TODO: move this to the functions that add the resources?
    reductions <- gets totalReductions 
    when (reductions == 0) $ modify initOriginMapWithAtoms
    omap <- gets originOfResources
    -- We now start the fixpoint calculation
    startFixpointReductions

startFixpointReductions :: ProverStateIO ()
startFixpointReductions = do
    -- We apply the reduction rules until we reach a fixed point
    initialContext <- gets env
    findFixpointStateIO reduceStateIO initialContext
    -- We have reached a dead-end, i.e., there are no enabled *focused* actions.
    -- We now test whether there are enabled *unfocused* actions. If there are any, 
    -- we move them to the context and we start again.
    -- TODO: Check with Gwenn if this is the desired behaviour.
    focusedActions <- gets env
    unfocusedActions <- gets unfocused 
    let existEnabledUnfocusedActions = not ( null ( listEnabledActions (focusedActions++unfocusedActions)))
    when (existEnabledUnfocusedActions) $ 
        do lift $ tellerWarning "We have reached a dead-end, but there are other available actions."
           modify moveUnfocusedToEnv
           modify (setFocusedReductionsTo 0)
           -- Ask if the user wants to proceed
           --answer <- askUserIfProceed
           --when (answer) $ startFixpointReductions
           startFixpointReductions
    
    -- Before we leave, let us clean the state by moving disabled unfocused actions to the environment 
    modify moveUnfocusedToEnv
    modify (setFocusedReductionsTo 0)

reduceStateIO :: Environment -> ProverStateIO Environment
reduceStateIO ts = do
    inDebugMode <- gets debugMode
    g <- gets granularity
    numFocusedReductions <- gets focusedReductions

    -- Not a fixpoint yet, but we have reached the granularity value. Bring back unfocused
    -- actions to the environment and reset the counter 'focusedReductions'. 
    -- TODO: What is this state called? Quiescence?
    when (g == numFocusedReductions) $ do 
            modify moveUnfocusedToEnv
            modify (setFocusedReductionsTo 0)
            lift $ tellerWarning "Granularity limit reached."
            --answer <- askUserIfProceed
            --when (answer) $ startFixpointReductions

    context <- gets env
    unfo    <- gets unfocused 
    let enabledActions = listEnabledActions context --(env state)
    when inDebugMode $
        (lift $ print $ "[DEBUG] ENVIRONMENT: " ++ show context) >>
        (lift $ print $ "[DEBUG] UNFOCUSED: " ++ show unfo) >>
        (lift $ print $ "[DEBUG] ENABLED ACTIONS: " ++ show enabledActions)

    -- If there are several available actions, let the user choose which one to
    -- reduce first
    -- TODO: and you are not focusing...
    when ((length enabledActions)>1) $ chooseActionToFocusOn enabledActions
    
    -- chooseActionToFocusOn changes the state, so let us get a new copy of the environment
    -- TODO, FIXME: This should improve. This style leads to programs difficult to debug!
    newEnv <- gets env
    
    -- We now try one reduction of each type, until we reach a fixpoint.
    tryReductionsStateIO reductions (linearizeTensorProducts newEnv)
         where reductions  = 
                [
                   -- reduceOfCourseLollyIO,
                   reduceLollyStateIO,
                   reduceWithStateIO,
                   reducePlusStateIO,
                   reduceOneStateIO
                 ]


-- split the next functions into IO (into CLI) + State
chooseActionToFocusOn :: [Term] -> ProverStateIO ()
chooseActionToFocusOn [] = return ()
chooseActionToFocusOn l = do
    lift $ printListOfActions l
    let sizeList = length l
    option <- lift $ getLine   -- TODO CHANGE FOR READLINE
    if (isValidActionChoice option sizeList) then 
     do
        let index = fst $ head (reads option :: [(Int, String)])
        let chosenAction = l!!index -- TODO: possibly unsafe? and the next steps not very efficient?
        state <- get
        let newEnv = chosenAction: (env (state) \\ l)
        let unFocus = (unfocused state) ++ (l \\ [chosenAction])
        put $ state { env = newEnv, unfocused = unFocus } 
        return ()
     else do 
        lift $ tellerWarning "Invalid Choice. Try again!"
        chooseActionToFocusOn l

   
isValidActionChoice :: String -> Int -> Bool
isValidActionChoice s n = 
    let value = (reads s :: [(Int, String)])
    in
    case value of
        []      -> False
        (i,_):_ -> if ((i>=0) && (i<n)) then True else False

tryReductionsStateIO :: [StateReduction] -> [Term] -> ProverStateIO [Term]
--tryReductionsStateIO (f:fs) t = tryReductionStateIO' f t >>= tryReductionsStateIO fs
tryReductionsStateIO (f:fs) t = do
    newEnv <- tryReductionStateIO' f t 
    modify (changeEnvTo newEnv) -- TODO: do I need this?
    let enabledActions = listEnabledActions newEnv

    -- If we are focusing, allow the choice of actions when there are several available
    gran <- gets granularity
    fred <- gets focusedReductions
    when ((fred<gran) && (length enabledActions)>1) $ chooseActionToFocusOn enabledActions

    newEnv <- gets env
    tryReductionsStateIO fs newEnv
tryReductionsStateIO []     t = return t

tryReductionStateIO' :: StateReduction -> [Term] -> ProverStateIO [Term]
tryReductionStateIO' f ls = return . fromMaybe ls =<< tryReductionStateIO f ls

tryReductionStateIO :: StateReduction -> [Term] -> ProverStateIO (Maybe [Term])
tryReductionStateIO f ls = go (point ls)
  where go [] = return Nothing
        go (x:xs) = do
            x' <- f x
            case x' of
              Just _  -> return x'
              Nothing -> go xs

-- TODO: organize the code below
-- (Move to Utils?)
point ls = go [] ls
  where go prev (x:next) = (x, prev ++ next) : go (x:prev) next
        go prev [] = []


pairs (x:y:xs) = (x, y):pairs xs
findRepeat :: Eq a => [a] -> a
findRepeat = fst . fromJust . find (uncurry (==)) . pairs


pointedMap f ls = map  f (point ls)
