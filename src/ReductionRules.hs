module ReductionRules where

import Control.Monad.State
import Data.List ((\\), intersect, nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import ProverState
import Syntax 
import Printer (showTerm)
import Term (detensor, linearizeTensorProducts, isSimple, isEnabledAction)
import UserIO (choose, chooseRandom, tellerWarning, tellerDebug)
import CGraph (getResourceNodeList, partitionResourceNodeList)

type StateReduction = (Term, Environment) -> ProverStateIO (Maybe Environment)

reduceLollyStateIO :: StateReduction
reduceLollyStateIO (t@(a :-@: b), ts) = 
    removeProductGiving' (\ts' -> b:ts') a b ts
reduceLollyStateIO (t@(OfCourse (a :-@: b)), ts) = do
    reduceLollyStateIO (a :-@:b, t:ts)
--    removeProductGiving' (\ts' -> t:ts') a b ts
    --removeProductGiving' (\ts' -> b:t:ts') a b ts
--    gran <- gets granularity
--    fres <- gets focusedReductions
--    if (gran-fres>0) then removeProductGiving' (\ts' -> b:t:ts') a b ts
--                     else return Nothing
reduceLollyStateIO _ = return $ Nothing


removeProductGiving' ::
  ([Term] -> [Term]) ->
  Term -> Term -> [Term] ->
  ProverStateIO (Maybe [Term])

removeProductGiving' f a b ts
  | isSimple a =
    --case removeProduct' a ts of
    -- TODO: make sure that myRemoveFunction is correct! and change its name to resourcesAreAvailable
    case myRemoveFunction a ts of
      Nothing   -> return Nothing
      Just ts'  -> do
        -- PROPERTY (RES_AVAILABLE): the action can execute, hence the map originOfResources has to contain all the needed resources
        state <- get

        -- Show message
        lift $ reduceMessage a b
        g <- gets granularity
        fr <- gets focusedReductions
--        lift $ putStrLn $ "Granularity " ++ (show g)
--        lift $ putStrLn $ "Focused Steps " ++ (show fr)

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
        --treductions <- gets totalReductions
        nodeCount <- gets _cGraphNode
        originRes <- gets originOfResources

        let needs = linearizeTensorProducts [a]
        let nodeList = getResourceNodeList state needs
        let flatNodeList = nub $ concatMap (\(_,_,c)->c) nodeList
        let (oneNode,multipleNodes) = partitionResourceNodeList nodeList

        -- FIXME: se precisar de 2 recursos e um for OR e o outro nao???
        if(multipleNodes==[]) then modify (addActionToTrace (nodeCount,flatNodeList,showTerm (a :-@: b)))
                 else do
                        modify (addActionToTrace (nodeCount,flatNodeList,"OR"))
                        modify incrementGraphNodeCount
                        nodeCount <- gets _cGraphNode
                        modify (addActionToTrace (nodeCount,[nodeCount-1],showTerm (a :-@: b)))

        -- The map is changed if oneNode is /= from []
        modify (changeMapNonOR oneNode)
        -- The map is changed if multipleNodes is /= from []
        modify (changeMapOR multipleNodes nodeCount)

        -- Change map with the newly introduced resources
        let introduces = linearizeTensorProducts [b]
        omap <- gets originOfResources
        let newMap = foldr (\k -> Map.insertWith (++) k [nodeCount]) omap introduces

        -- TODO: use modify
        state <- get
        put $ state {originOfResources = newMap}
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
       modify (changeEnvTo (t:ts))
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
       modify (changeEnvTo (t:ts))
       return $ Just (t:ts) 
 | (isEnabledAction ts a) && (not (isEnabledAction ts b)) = 
    do modify (changeEnvTo (a:ts))
       return $ Just (a:ts) 
 | (not (isEnabledAction ts a)) && (isEnabledAction ts b) = 
    do modify (changeEnvTo (b:ts))
       return $ Just (b:ts) 
 | otherwise = return Nothing
reducePlusStateIO _ = return Nothing


-- | 'reduceOneStateIO' removes any occurence of One from the environment.
reduceOneStateIO :: StateReduction
reduceOneStateIO (One, ts) = do
    modify (changeEnvTo ts)
    return $ Just ts
reduceOneStateIO _ = return Nothing


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
    let newAtoms = linearizeTensorProducts [atoms]
        newEnv   = linearizeTensorProducts env
        int = intersect newEnv newAtoms
    in 
        if (newAtoms \\ int == [])
        then Just $ newEnv \\ newAtoms
        else Nothing

-- TODO: change this to CLI?
reduceMessage :: Term -> Term -> IO ()
reduceMessage a b = tellerWarning $ concat ["reducing: ",   showTerm (a :-@: b),
                                            ", removing: ", showTerm a,
                                            ", adding: ",   showTerm b]

lollyTensorWarning :: IO (Maybe a)
lollyTensorWarning = do 
      tellerWarning "Warning: lolly LHSs must be simple tensor products"
      return $ Nothing



