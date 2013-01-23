module ReductionRules where

import Control.Monad.State
import Data.List ((\\), intersect, nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import ProverState
import Syntax 
import Printer (showTerm)
import Term (detensor, linearizeTensorProducts, isSimple)
import UserIO (choose, chooseRandom, tellerWarning, tellerDebug)
import CGraph (getResourceNodeList, partitionResourceNodeList)

type StateReduction = (Term, Environment) -> ProverStateIO (Maybe Environment)

reduceLollyStateIO :: StateReduction
reduceLollyStateIO (t@(a :-@: b), ts) = 
    removeProductGiving' (\ts' -> b:ts') a b ts
reduceLollyStateIO (t@(OfCourse (a :-@: b)), ts) = 
    removeProductGiving' (\ts' -> b:t:ts') a b ts
reduceLollyStateIO _ = return $ Nothing


removeProductGiving' ::
  ([Term] -> [Term]) ->
  Term -> Term -> [Term] ->
  ProverStateIO (Maybe [Term])

removeProductGiving' f a b ts
  | isSimple a =
    --case removeProduct' a ts of
    -- TODO: make sure that myRemoveFunction is correct!
    case myRemoveFunction a ts of
      Nothing   -> return Nothing
      Just ts'  -> do
        -- PROPERTY (RES_AVAILABLE): the action can execute, hence the map originOfResources has to contain all the needed resources
        state <- get

        -- Show message
        lift $ reduceMessage a b

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


reduceMessage :: Term -> Term -> IO ()
reduceMessage a b = tellerWarning $ concat ["reducing: ",   showTerm (a :-@: b),
                                       ", removing: ", showTerm a,
                                       ", adding: ",   showTerm b]

lollyTensorWarning :: IO (Maybe a)
lollyTensorWarning = do 
      tellerWarning "warning: lolly LHSs must be simple tensor products"
      return $ Nothing


-- TODO: unify reduceWith and reducePlus, parameterizing the choice function
reduceWithStateIO :: StateReduction
reduceWithStateIO (term@(a :&: b), ts) = 
    do t <- lift $ choose a b  -- ask the user what action to choose
       state <- get
       put $ state {env = t:ts } 
       return $ Just (t:ts) 
reduceWithStateIO _ = return Nothing

reducePlusStateIO :: StateReduction
reducePlusStateIO (term@(a :+: b), ts) = 
    do t <- lift $ chooseRandom a b  
       state <- get
       put $ state {env = t:ts } 
       return $ Just (t:ts) 
reducePlusStateIO _ = return Nothing


reduceOneStateIO :: StateReduction
reduceOneStateIO (One, ts) = do
    state <- get
    put $ state {env = ts}
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
