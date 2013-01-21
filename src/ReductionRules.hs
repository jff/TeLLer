module ReductionRules where

import Control.Monad.State
import Data.List ((\\), intersect)

import ProverState
import Syntax 
import Printer (showTerm)
import Term (detensor, linearizeTensorProducts, isSimple)
import UserIO (choose, chooseRandom)

type StateReduction = (Term, Environment) -> ProverStateIO (Maybe Environment)

reduceLollyStateIO :: StateReduction
reduceLollyStateIO (t@(a :-@: b), ts) = do
    state <- get
    put $ state { actionTrace = (actionTrace state) ++ [t] }
    removeProductGiving' (\ts' -> b:ts') a b ts
reduceLollyStateIO (t@(OfCourse (a :-@: b)), ts) = do
    state <- get
    put $ state { actionTrace = (actionTrace state) ++ [t] }
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
        state <- get
        lift $ reduceMessage a b
        -- Terms b were just introduced. If b enables unfocused actions,
        -- bring them to the environment.
        actionsToFocus <- focusActionsEnabledBy b
        state <- get
        put $ state { env = (f ts')++actionsToFocus}
        increaseNumberOfReductionsBy 1
        return $ Just ((f ts') ++ actionsToFocus)

  | otherwise = lift $ lollyTensorWarning


{--
removeProductGiving ::
  ([Term] -> [Term]) ->
  Term -> Term -> [Term] ->
  IO (Maybe [Term])

removeProductGiving f a b ts
  | isSimple a =
    case removeProduct' a ts of
      Nothing   -> return Nothing
      Just ts'  -> do
        reduceMessage a b
        return $ Just (f ts')

  | otherwise = lollyTensorWarning
--}

reduceMessage :: Term -> Term -> IO ()
reduceMessage a b = putStrLn $ concat ["reducing: ",   showTerm (a :-@: b),
                                       ", removing: ", showTerm a,
                                       ", adding: ",   showTerm b]

lollyTensorWarning :: IO (Maybe a)
lollyTensorWarning = do 
      putStrLn "warning: lolly LHSs must be simple tensor products"
      return $ Nothing


reduceWithStateIO :: StateReduction
reduceWithStateIO (term@(a :&: b), ts) = 
    do t <- lift $ choose a b  -- ask the user what action to choose
       state <- get
--       put $ state {env = (t:(env state)) \\ [term]} -- change the environment
       put $ state {env = t:ts } 
       return $ Just (t:ts) -- TODO: DO I NEED A RETURN TYPE?
reduceWithStateIO _ = return Nothing

reducePlusStateIO :: StateReduction
reducePlusStateIO (term@(a :+: b), ts) = 
    do t <- lift $ chooseRandom a b  -- ask the user what action to choose
       state <- get
--       put $ state {env = (t:(env state)) \\ [term]} -- change the environment
       put $ state {env = t:ts } 
       return $ Just (t:ts) -- TODO: DO I NEED A RETURN TYPE?
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
 


