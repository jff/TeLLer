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
    -- BT

    tellAllStories <- gets tellAllStories
    when tellAllStories $ do
        stack <- gets btStack
        graphs <- gets btTraces
        if((length stack) > 0) then do
                                     trace <- gets actionTrace
                                     let next = head stack
                                     -- set the state to next, except for the stack
                                     allTraces <- gets btTraces
                                     put $ next {btStack = tail stack, btTraces = trace:allTraces}
                                     startTeLLer
                                        
                               else do  
                                     trace <- gets actionTrace
                                     state <- get
                                     put $ state {btTraces = trace:(btTraces state)}
                                     --g <- gets btTraces
                                     --lift $ putStrLn $ "ALL TRACES: "++ show g
                                     --lift $ putStrLn $ show g

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
    when ((length enabledActions)>1) $ chooseActionToFocusOn enabledActions True
    
    -- chooseActionToFocusOn changes the state, so let us get a new copy of the environment
    -- TODO, FIXME: This should improve. This style leads to programs difficult to debug!
    newEnv <- gets env
    
    -- We now try one reduction of each type, until we reach a fixpoint.
    tryReductionsStateIO reductions (linearizeTensorProducts newEnv)
         where reductions  = 
                [
                   reduceLollyStateIO,
                   reduceWithStateIO,
                   reducePlusStateIO,
                   reduceOneStateIO
                 ]


-- split the next functions into IO (into CLI) + State
-- Second parameter: True if running for the first time or if list of actions changed
chooseActionToFocusOn :: [Term] -> Bool -> ProverStateIO ()
chooseActionToFocusOn [] _ = return ()
chooseActionToFocusOn l  printListActions = do
    when (printListActions) $ lift $ printListOfActions l

    -- JFF: Cindy wants to be able to print the state and to add new resources at this point
    -- TODO: refactor the code...
    
    when (printListActions) $ lift $ tellerPrintLn "p) Print environment"
    when (printListActions) $ lift $ tellerPrintLn "+-) Add/Remove resources (e.g. +A A-@B A-@C)"
    when (printListActions) $ lift $ tellerPrintLn "choicepoints) List choice points"
    when (printListActions) $ lift $ tellerPrintLn "goto n) Go to choice point n"
    option <- lift $ getLine   -- TODO CHANGE FOR READLINE

    -- user selects printing option
    when ((head option) == 'p') $ do
        state <- get
        lift (tellerPrintLn (showState state)) 
    -- TODO: something weird is happening with IO (possibly related with Readline): p is being read
    -- twice!

    when ((head option) == '+') $ do
        changeEnvWith addToEnv (drop 1 option)

    when ((head option) == '-') $ do
        changeEnvWith removeFromEnv (drop 1 option)

    when (option == "choicepoints") $ do
        cpoints <- gets choicePoints
        let l = map (\(n,f,s,t) -> (n,f,s)) cpoints
        if(null l) then lift (tellerPrintLn "No choice points to show!") 
                   else lift (tellerPrintLn (show l)) 

    when ((take 5 option) == "goto ") $ do
        let index = fst $ head (reads (drop 5 option) :: [(Int,String)])
        cpoints <- gets choicePoints
        let npoints = length cpoints
        let newState = (\(n,f,s,t)->t) $ cpoints!!(npoints-1-index)
        put newState 


    -- Adding new actions can add new enabled actions!
    context <- gets env
    let initialListActions = l
    let l = listEnabledActions context 
    let sizeList = length l 

    if (isValidActionChoice option sizeList) then 
     do
        let index = fst $ head (reads option :: [(Int, String)])
        let chosenAction = l!!index -- TODO: possibly unsafe? and the next steps not very efficient?
        state <- get
        let newEnv = chosenAction: (env (state) \\ l)
        let unFocus = (unfocused state) ++ (l \\ [chosenAction])

        -- There was a choice, so add this state to choicePoints
        state <- get
        let lastActionName = (\(f,s,t) -> t) $ (\l -> if (null l) then (0,[],"Beginning") else last l) $ filter (\(_,_,nm)->(take 7 nm) /= (take 7 "\\emptyset")) $ actionTrace state 
        let currentChoicePoints = choicePoints state
        put (state {choicePoints = (length currentChoicePoints, lastActionName, map showAction l, state): currentChoicePoints})

        -- Saved states
        tellAllStories <- gets tellAllStories
        when tellAllStories $ do
            savedState <- get
            let alternatives = [ savedState {env = notChosen: (env (savedState)\\l), 
                                             unfocused = (unfocused savedState) ++ (l\\[notChosen])} 
                                 | notChosen <- l\\[chosenAction]]
    --        lift $ putStrLn $ "ALTS " ++ show (map env alternatives)
            state <- get
            put (state {btStack = alternatives++(btStack state)})
        -- End of saved states

        state <- get
        put $ state { env = newEnv, unfocused = unFocus } 
        return ()
     else do 
        lift $ tellerWarning $ "Choose an action from 0 to " ++ (show (sizeList-1) ++ " to proceed!")
        chooseActionToFocusOn l (l/=initialListActions)

   
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
    when ((fred<gran) && (length enabledActions)>1) $ chooseActionToFocusOn enabledActions True

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
