module Reductions where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import System.Random

import Util
import Syntax
import Printer
import ProverState
import Term
import RewriteRules (simplify)

type Reduction   = (Term, [Term]) -> Maybe [Term]
type IOReduction = (Term, [Term]) -> IO (Maybe [Term])
type StateReduction = (Term, [Term]) -> ProverStateIO (Maybe [Term])


--reduce'     = findFixpoint   reduce
--reduceIO' t = findFixpointIO reduceIO t
--reduceStateIO' t = findFixpointStateIO reduceStateIO (map simplify t)
startTeLLer = do
    state <- get
    -- First, we apply apply some simplification rules to the environment
    put $ state { env = map simplify (env state) }
    startFixpointReductions


startFixpointReductions = do
    -- We apply the reduction rules until we reach a fixed point
    state <- get
    findFixpointStateIO reduceStateIO (env state)
    -- we have reached a dead-end; test if there are unfocused actions
    state <- get
    let focusedActions = env state
    let unfocusedActions = unfocused state
    let existEnabledUnfocusedActions = not ( null ( listEnabledActions (focusedActions++unfocusedActions)))
    when (existEnabledUnfocusedActions) $ 
        do lift $ flushStrLn "We have reached a dead-end, but there are other available actions."
           put $ state { env = (env state) ++ (unfocused state),  unfocused = [] }
           -- Ask if the user wants to proceed
           --answer <- askUserIfProceed
           --when (answer) $ startFixpointReductions
           startFixpointReductions
    
    -- Clean the state: move disabled unfocused actions to the environment before we leave
    state <- get
    put $ state { env = (env state) ++ (unfocused state),  unfocused = [] }

askUserIfProceed :: ProverStateIO Bool
askUserIfProceed = do
    lift $ flushStrLn "Do you want to proceed? (y/n)"
    answer <- lift $ getLine
    case answer of
        ('y':_) -> return True
        ('n':_) -> return False
        _       ->  (lift $ flushStrLn "Invalid choice! Try again.") >> askUserIfProceed
    

reduceStateIO :: [Term] -> ProverStateIO [Term]
reduceStateIO ts = do
    state <- get
    let inDebugMode = debugMode state

    -- Not a fixpoint yet, but we have reached the granularity value. Bring back unfocused
    -- actions to the environment. TODO: What is this state called? Quiescence?
    when (granularity state == focusedReductions state) $ do 
            put $ state { env = (env state) ++ (unfocused state), 
                          unfocused = [], 
                          focusedReductions = 0  }
            lift $ putStrLn "Granularity limit reached."
            --answer <- askUserIfProceed
            --when (answer) $ startFixpointReductions

    state <- get
    let enabledActions = listEnabledActions (env state)
    when inDebugMode $
        (lift $ print $ "[DEBUG] ENVIRONMENT: " ++ show (env state)) >>
        (lift $ print $ "[DEBUG] ENABLED ACTIONS: " ++ show enabledActions)

    -- If there are several available actions, let the user choose which one to
    -- reduce first
    when ((length enabledActions)>1) $ chooseActionToFocusOn enabledActions
    
    -- chooseActionToFocusOn changes the state, so let us get a new copy
    state <- get 
    let newEnv = env (state)
    tryReductionsStateIO reductions (linearizeTensorProducts newEnv)
         where reductions  = 
                [
                   -- reduceOfCourseLollyIO,
                   reduceLollyStateIO,
                   reduceWithStateIO,
                   reducePlusStateIO,
                   reduceOneStateIO
--                   reducePlusIO,
--                   reduceOneIO
                 ]
-- NOTE: the prover tries one of each type, until it reaches a fixpoint.



--reduce   :: [Term] -> [Term]
--reduceIO :: [Term] -> IO [Term]

--reduce   ts = tryReduction' reduceLolly $ concatMap detensor ts
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
        lift $ flushStrLn "Invalid Choice. Try again!"
        chooseActionToFocusOn l

printListOfActions l = do
    flushStrLn "There are several actions available. Please choose one of the following:"
    let f = \(n,a) -> flushStr ((show n) ++ ") ") >> flushStrLn (show a)
    sequence_ $ map f (zip [0..] l)
    
isValidActionChoice :: String -> Int -> Bool
isValidActionChoice s n = 
    let value = (reads s :: [(Int, String)])
    in
    case value of
        []      -> False
        (i,_):_ -> if ((i>=0) && (i<n)) then True else False

tryReductionsStateIO :: [StateReduction] -> [Term] -> ProverStateIO [Term]
tryReductionsStateIO (f:fs) t = tryReductionStateIO' f t >>= tryReductionsStateIO fs
tryReductionsStateIO []     t = return t

{--
tryReductionsIO :: [IOReduction] -> [Term] -> IO [Term]
tryReductionsIO (f:fs) t = tryReductionIO' f t >>= tryReductionsIO fs
tryReductionsIO []     t = return t
--}

{--
reduceLolly :: Reduction
reduceLolly (a :-@: b, ts)
  | isSimple a = do ts' <- removeProduct' a ts; Just (b:ts')
  | otherwise  = error "lolly LHSs must be simple tensor products"
reduceLolly _  = Nothing
--}

{--
reduceLollyIO :: IOReduction
reduceLollyIO (a :-@: b, ts) =
   removeProductGiving (\ts' -> b:ts') a b ts

reduceLollyIO (t@(OfCourse (a :-@: b)), ts) =
   removeProductGiving (\ts' -> b:t:ts') a b ts

reduceLollyIO _ = return $ Nothing
--}

reduceLollyStateIO :: StateReduction
reduceLollyStateIO (a :-@: b, ts) =
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
        state <- get
        lift $ reduceMessage a b
        -- Terms b were just introduced. If b enables unfocused actions,
        -- bring them to the environment.
        actionsToFocus <- focusActionsEnabledBy b
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


{--
reducePlusIO :: IOReduction
reducePlusIO (a :+: b, ts) = do t <- chooseRandom a b
                                return $ Just (t:ts)
reducePlusIO _ = return Nothing
--}

reduceOneStateIO :: StateReduction
reduceOneStateIO (One, ts) = do
    state <- get
    put $ state {env = ts}
    return $ Just ts
reduceOneStateIO _ = return Nothing

{--
reduceOneIO :: IOReduction
reduceOneIO (One, ts) = return $ Just ts
reduceOneIO _ = return Nothing
--}

reduceOfCourseLollyIO :: IOReduction
reduceOfCourseLollyIO (OfCourse (a :-@: b), ts) =
  if (a :-@: b) `elem` ts
  then return $ Nothing
  else return $ Just ((a :-@: b):OfCourse (a :-@: b):ts)

reduceOfCourseLollyIO _ = return Nothing

choose :: Term -> Term -> IO Term
choose s t = do putStrLn "Please choose:"
                putStrLn $ "\t1) " ++ (showTerm s)
                putStrLn $ "\t2) " ++ (showTerm t)
                line <- getLine
                case line of
                  "1" -> return s
                  "2" -> return t
                  _   -> putStrLn "Invalid choice" >> choose s t

chooseRandom s t = do
  x <- randomRIO (0, 1)
  let t' = case (x :: Int) of
             0 -> s
             1 -> t
  putStrLn $ concat [
      "TeLLer's random choice: ", showTerm t', " from ",
      showTerm s, " or ", showTerm t
    ]
  return t'

isSimple ts = all isAtom (detensor ts)

isAtom (Atom _) = True
isAtom _ = False

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


--tryReduction'   f ls = fromMaybe ls (tryReduction f ls)
--tryReductionIO' f ls = return . fromMaybe ls =<< tryReductionIO f ls
tryReductionStateIO' f ls = return . fromMaybe ls =<< tryReductionStateIO f ls

pointedMap f ls = map  f (point ls)

--tryReduction   :: Reduction   -> [Term] -> Maybe [Term]
--tryReductionIO :: IOReduction -> [Term] -> IO (Maybe [Term])
tryReductionStateIO :: StateReduction -> [Term] -> ProverStateIO (Maybe [Term])

--tryReduction   f ls = msum (pointedMap f ls)
tryReductionStateIO f ls = go (point ls)
  where go [] = return Nothing
        go (x:xs) = do
            x' <- f x
            case x' of
              Just _  -> return x'
              Nothing -> go xs

point ls = go [] ls
  where go prev (x:next) = (x, prev ++ next) : go (x:prev) next
        go prev [] = []


pairs (x:y:xs) = (x, y):pairs xs
findRepeat :: Eq a => [a] -> a
findRepeat = fst . fromJust . find (uncurry (==)) . pairs
--findFixpoint f = findRepeat . iterate f

findFixpointStateIO :: Eq a => (a -> ProverStateIO a) -> a -> ProverStateIO a
findFixpointStateIO f x =
  do x' <- f x
     case x' == x of
       True  -> return x'
       False -> findFixpointStateIO f x'


-- The following function replaces removeProductGiving'.
-- Its complexity is still quadratic, but it is much easier
-- to read!
myRemoveFunction :: Term -> [Term] -> Maybe [Term]
myRemoveFunction atoms env = 
    let newAtoms = linearizeTensorProducts [atoms]
        newEnv   = linearizeTensorProducts env
        int = intersect newAtoms newEnv
    in 
        if (int==newAtoms)
        then Just $ newEnv \\ newAtoms
        else Nothing
        
