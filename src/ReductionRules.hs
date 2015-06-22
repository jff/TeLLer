module ReductionRules where

import Control.Monad.State
import Data.List ((\\), intersect, nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import ProverState
import Syntax 
import Printer (showTerm, showAction)
import Term (detensor, linearizeTensorProducts, isSimple, isEnabledAction)
import UserIO (choose, chooseRandom, tellerWarning, tellerDebug, readStringFromUser)
import CGraph (getResourceNodeList, partitionResourceNodeList)
import Util (third)

type StateReduction = (Term, Environment) -> ProverStateIO (Maybe Environment)

reduceLollyStateIO :: StateReduction
reduceLollyStateIO (t@((:-@:) a b _), ts) = 
    removeProductGiving' (\ts' -> b:ts') t ts
reduceLollyStateIO (t@(OfCourse ((:-@:) a b d)), ts) = do
    reduceLollyStateIO ((:-@:) a b d, t:ts)
reduceLollyStateIO _ = return $ Nothing


removeProductGiving' ::
  ([Term] -> [Term]) ->
  Term -> [Term] ->
  ProverStateIO (Maybe [Term])

removeProductGiving' f ((:-@:) a b actionDesc) ts
  | isSimple a =
    --case removeProduct' a ts of
    -- TODO: make sure that myRemoveFunction is correct! and change its name to resourcesAreAvailable
    case myRemoveFunction a ts of
      Nothing   -> return Nothing
      Just ts'  -> do
        -- PROPERTY (RES_AVAILABLE): the action can execute, hence the map originOfResources has to contain all the needed resources
        state <- get

        -- Show message
        lift $ reduceMessage a b actionDesc
        g <- gets granularity
        fr <- gets focusedReductions

        -- Terms b were just introduced. If b enables unfocused actions,
        -- bring them to the environment.
        actionsToFocus <- focusActionsEnabledBy b

        -- TODO: change the following to modify
        state <- get
        put $ state { env = (f ts')++actionsToFocus}

        -- Increase number of reductions *and* the graph node count
        increaseNumberOfReductionsBy 1 -- TODO: change this to modify
        modify incrementGraphNodeCount

        -- change trace and map
        state <- get
        nodeCount <- gets _cGraphNode
        originRes <- gets originOfResources

        let needs = linearizeTensorProducts [a]
        let nodeList = getResourceNodeList state needs
        let flatNodeList = nub $ concatMap (\(_,_,c)->c) nodeList
        let (multipleNodes, oneNode) = partitionResourceNodeList nodeList

        let nodeLabel = fromMaybe (showTerm ((:-@:) a b Nothing)) actionDesc
        if(multipleNodes==[]) then modify (addActionToTrace (nodeCount,flatNodeList,nodeLabel))
                 else do
                        let multipleNodesNonDup = nub (concatMap third multipleNodes)
                        modify (addActionToTrace (nodeCount,multipleNodesNonDup ,"OR"))
                        modify incrementGraphNodeCount
                        nodeCount <- gets _cGraphNode
                        
                        -- If this action also depends of resources that originate from a single node, we need
                        -- to establish those connections in the causality graph
                        let oneNodeNonDup = nub (concatMap third oneNode)
                        if (oneNodeNonDup == []) then modify (addActionToTrace (nodeCount,[nodeCount-1],nodeLabel))
                                                 else modify (addActionToTrace (nodeCount,(nodeCount-1):oneNodeNonDup,nodeLabel))

        nodeCount <- gets _cGraphNode
        -- The map is changed if oneNode is /= from []
        modify (changeMapNonOR oneNode)
        -- The map is changed if multipleNodes is /= from []
        modify (changeMapOR multipleNodes (nodeCount-1))

        -- Change map with the newly introduced resources
        let introduces = linearizeTensorProducts [b]
        omap <- gets originOfResources
        let newMap = foldr (\k -> Map.insertWith (++) k [nodeCount]) omap introduces

        -- TODO: use modify
        state <- get
        put $ state {originOfResources = newMap}
        lift $ tellerWarning "p) Print environment"
        option <- lift $ getLine   -- TODO CHANGE FOR READLINE
        when ((head option) == 'p') $ do
            state <- get
            lift (tellerWarning (showState state)) 
    -- TODO: something weird is happening with IO (possibly related with Readline): p is being read
    -- twice!



        return $ Just ((f ts') ++ actionsToFocus)

  | otherwise = lift $ lollyTensorWarning


-- | 'reduceWithStateIO' applies the reduction rule for &.
--   The reduction only happens if both arguments of & are enabled.
reduceWithStateIO :: StateReduction
reduceWithStateIO (term@(a :&: b), ts) 
 -- Perform only if the choice is enabled (i.e., both actions are enabled)
 | isEnabledAction ts term =
    do 
       t <- lift $ choose (term:ts) a b  -- ask the user what action to choose

       -- There was a choice, so add this state to choicePoints
       state <- get
       let lastActionName = (\(f,s,t) -> t) $ last $ actionTrace state 
       let currentChoicePoints = choicePoints state
       put (state {choicePoints = (length currentChoicePoints, lastActionName, map showAction [a,b], state): currentChoicePoints})


       savedState <- get
       let alternative = savedState {env = ([a,b]\\[t])++ts }
       modify (changeEnvTo (t:ts))
       tellAllStories <- gets tellAllStories
       when tellAllStories $ do
           state <- get
--           lift $ putStrLn $ "STACK: " ++ show (env alternative)
           put (state {btStack = alternative:(btStack state)})
       return $ Just (t:ts) 
 | otherwise = return Nothing
reduceWithStateIO _ = return Nothing

-- | 'reduceWithPlusStateIO' applies the reduction rule for +.
--   If both arguments are enabled, TeLLer chooses one randomly.
--   If only one of the arguments is enabled, that argument is chosen.
--   Otherwise, the state is not changed.
reducePlusStateIO :: StateReduction
reducePlusStateIO (term@(a :+: b), ts) 
 -- Perform only if one of the choices is enabled. If both are available, choose randomly.
 | (isEnabledAction ts a) && (isEnabledAction ts b) = 
    do t <- lift $ chooseRandom a b  
       savedState <- get
       let alternative = savedState {env = ([a,b]\\[t])++ts }
       lift $ putStrLn $ "Alt:" ++ show (env alternative)
       modify (changeEnvTo (t:ts))

       tellAllStories <- gets tellAllStories
       when tellAllStories $ do
           state <- get
           put (state {btStack = alternative:(btStack state)})
       return $ Just (t:ts) 
 | (isEnabledAction ts a) && (not (isEnabledAction ts b)) = 
    do savedState <- get
       let alternative = savedState {env = (b:ts) }
       modify (changeEnvTo (a:ts))

       tellAllStories <- gets tellAllStories
       when tellAllStories $ do
           state <- get
           put (state {btStack = alternative:(btStack state)})
       return $ Just (a:ts) 
 | (not (isEnabledAction ts a)) && (isEnabledAction ts b) = 
    do savedState <- get
       let alternative = savedState {env = (a:ts) }
       modify (changeEnvTo (b:ts))

       tellAllStories <- gets tellAllStories
       when tellAllStories $ do
           state <- get
           put (state {btStack = alternative:(btStack state)})
       return $ Just (b:ts) 
 | otherwise = return Nothing
reducePlusStateIO _ = return Nothing


-- | 'reduceOneStateIO' removes any occurence of One from the environment.
reduceOneStateIO :: StateReduction
reduceOneStateIO (One, ts) = do
    modify (changeEnvTo ts)
    return $ Just ts
reduceOneStateIO _ = return Nothing

-- | 'reduceOfCourseAtom' introduces an atom in case it isn't already there
{--
reduceOfCourseAtomStateIO :: StateReduction
reduceOfCourseAtomStateIO (OfCourse t@(Atom a), ts) = do
    if t `elem` ts then return Nothing
                   else do modify (changeEnvTo (t:(OfCourse t):ts))
                           return $ Just (t:(OfCourse t):ts)
reduceOfCourseAtomStateIO _ = return Nothing
--}


{--
reduceOfCourseLollyIO :: IOReduction
reduceOfCourseLollyIO (OfCourse (a :-@: b), ts) =
  if (a :-@: b) `elem` ts
  then return $ Nothing
  else return $ Just ((a :-@: b):OfCourse (a :-@: b):ts)

reduceOfCourseLollyIO _ = return Nothing
--}


{--
removeProduct'  :: Term -> [Term] -> Maybe [Term]
removeProduct   :: [String] -> [String] -> Maybe [String]

removeProduct' t ts =
  let deatom (Atom s) = s
      used = map deatom (detensor t)
      (atoms, rest) = partition isAtom ts
      have = map deatom atoms
      left = removeProduct (sort used) (sort have)
  in do atoms' <- left
        return (rest ++ map Atom atoms')

removeProduct (x:xs) (t:ts)
  | x == t    = removeProduct xs ts
  | otherwise = do ts' <- removeProduct (x:xs) ts
                   Just (t:ts')
removeProduct [] ts = Just ts
removeProduct _  [] = Nothing
--}

-- The following function replaces removeProductGiving'.
-- Its complexity is still quadratic, but it is much easier
-- to read!
-- FIXME, TODO: REVIEW, BECAUSE BECAUSE INTERSECT DOES NOT WORK AS EXPECTED! IT IS NOT COMMUTATIVE.
myRemoveFunction :: Term -> [Term] -> Maybe [Term]
myRemoveFunction atoms env = 
    let need = linearizeTensorProducts [atoms] -- what we need
        available   = linearizeTensorProducts env     -- what we have
        int = intersect available need
        intPersistent = intersect available (map OfCourse need) -- check for persistent resources
        simpleNotAvailable = need \\ int
        persistentNeeded = map OfCourse simpleNotAvailable
        nonExistent = persistentNeeded \\ (concat . replicate (length persistentNeeded)) intPersistent -- nonExistent = map ! (need\\int)\\intPersistent
    in 
        --if (need \\ int) == [])
        if (nonExistent == []) -- then, we have all that we need!
        then Just $ available \\ need
        else Nothing

-- TODO: change this to CLI?
-- If action is named, shows the name; otherwise it shows reduction
reduceMessage :: Term -> Term -> Maybe String -> IO ()
reduceMessage a b Nothing = tellerWarning $ concat ["reducing: ",   showTerm ((:-@:) a b Nothing),
                                                    ", removing: ", showTerm a,
                                                    ", adding: ",   showTerm b]
reduceMessage a b (Just name) = do 
    tellerWarning (concat ["Action performed: ", name])
    readStringFromUser "" -- TODO: this was requested by Cindy, but it's not great when interacting with CLI
    return ()

lollyTensorWarning :: IO (Maybe a)
lollyTensorWarning = do 
      tellerWarning "Warning: lolly LHSs must be simple tensor products"
      return $ Nothing



