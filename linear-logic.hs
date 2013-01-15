module Main where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Applicative 
import System.Random
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)

import Bag
import Syntax
import Parser
import Printer
import RewriteRules

type Reduction   = (Term, [Term]) -> Maybe [Term]
type IOReduction = (Term, [Term]) -> IO (Maybe [Term])

type Granularity = Int
data ProverState = ProverState 
    { env :: [Term],
      granularity :: Granularity,
      debugMode :: Bool
    }
initialState = ProverState { env = [], granularity = 0, debugMode = True }

linearizeTensorProducts :: [Term] -> [Term]
linearizeTensorProducts = concatMap detensor 

changeEnvTo :: [Term] -> ProverState -> ProverState
changeEnvTo newEnv state = state { env = linearizeTensorProducts newEnv } 

addToEnv :: [Term] -> ProverState -> ProverState
addToEnv resources state = state {env = (env state)++(linearizeTensorProducts resources)} 

-- TODO: removeFromEnv is quadratic!; can this improve?
removeFromEnv :: [Term] -> ProverState -> ProverState
removeFromEnv resources state = state {env = (env state) \\ (linearizeTensorProducts resources) }

listEnabledActions :: [Term] -> [Term]
listEnabledActions env = filter (isEnabledAction env) env

isEnabledAction :: [Term] -> Term -> Bool
isEnabledAction _   (_ :$: _)    = True
isEnabledAction env (t1 :-@: t2) = and $  elem <$> (linearizeTensorProducts [t1]) <*> [env]
isEnabledAction _ _              = False


changeDebugMode :: ProverState -> ProverState
changeDebugMode state = state {debugMode = not (debugMode state)} 

type ProverStateIO a = StateT ProverState IO a


-- ask user for string
-- TODO: Improve reliability; tabs/auto-complete (readline), etc.
readStringFromUser :: String -> IO String
readStringFromUser query = do
  putStr query
  hFlush stdout
  answer <- getLine
  return answer

toggleDebugMode :: ProverStateIO ProverState
toggleDebugMode = do
    state <- get
    let inDebugMode = debugMode state
    if inDebugMode then lift (putStrLn "Debug mode turned off.")
                   else lift (putStrLn "Debug mode turned on.")
    return (changeDebugMode state)

loadFile :: ProverStateIO ProverState
loadFile = do
    fileName <- lift $ readStringFromUser "Load file: "
    state <- get
    fileExists <- lift $ doesFileExist fileName
    if (fileExists) then do fileContents <- lift $ readFile fileName 
                            return (changeEnvTo (tts' fileContents) state)--change env with tts' fileContents
                    else lift (putStrLn "ERROR: File does not exist!") >> return state

removeResource :: ProverStateIO ProverState
removeResource = do
    resources <- lift $ readStringFromUser "Enter resources to remove: "
    state <- get
    case parse term "<interactive>" resources of
     Left err -> do
                  lift $ putStrLn "ERROR: Parsing error. Please try again." 
                  return state
     Right r -> return (removeFromEnv (tts' resources) state)



addResource :: ProverStateIO ProverState
addResource = do
    resources <- lift $ readStringFromUser "Enter resources to add: "
    state <- get
    case parse term "<interactive>" resources of
     Left err -> do
                  lift $ putStrLn "ERROR: Parsing error. Please try again." 
                  return state
     Right r -> return (addToEnv (tts' resources) state)

startReductions :: ProverStateIO ProverState
startReductions = do
    state <- get
    let inDebugMode = debugMode state
    when inDebugMode $
        do lift $ print $ "[DEBUG] ALL ACTIONS: " ++ show ((env state)) 
           lift $ print $ "[DEBUG] ENABLED ACTIONS: " ++ show (listEnabledActions (env state))
    -- TODO: ASK FOR ENABLED ACTIONS
    newEnv <- lift $ reduceIO' (map simplify (env state)) 
    let newState = state {env = newEnv}
    put newState
    lift $ putStrLn "End of story, no more reductions found for:"
    return newState -- $ state { env = newEnv }
    
printEnv state = putStrLn $ "\n[DEBUG] Current resources: \n" ++ showTerms (env state) ++ "\n"

getFirstChar :: String -> Char
getFirstChar []    = ' '
getFirstChar (h:t) = h

-- the main loop for interacting with the user
mainLoop :: ProverState -> IO ProverState
mainLoop state = do
  let inDebugMode = debugMode state
  when inDebugMode $ printEnv state 
  putStr "Command [d+-lpsqrh]: "
  hFlush stdout
  comm <- getLine
  let c = getFirstChar comm
  putStr "\n"
  continue <- case c of
    'd' -> do newEnv <- evalStateT toggleDebugMode state; return (Just newEnv)
    '+' -> do newEnv <- evalStateT addResource state; return (Just newEnv)
    '-' -> do newEnv <- evalStateT removeResource state; return (Just newEnv)
    'l' -> do newEnv <- evalStateT loadFile state; return (Just newEnv)
    'p' -> if inDebugMode then return (Just state) else printEnv state >> return (Just state)
    'q' -> return Nothing
    'r' -> return (Just initialState)
    's' -> do newEnv <- evalStateT startReductions state; return (Just newEnv) --putStrLn "TODO: not implemented yet!" >> return (Just state)
    'h' -> putStrLn help >> return (Just state)
    '\n' -> return (Just state)
    _   -> do 
             putStrLn $ "Command '" ++ [c] ++ "' not recognized."
             return (Just state)
  maybe (return state) mainLoop continue

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> do fileExists <- doesFileExist f
              if(fileExists) then printWelcomeMessage >> createEnvFromFile f >>= mainLoop
                             else printWelcomeMessage >> putStrLn "ERROR: File passed in command line does not exist." >> mainLoop initialState
              return ()
    []  -> do printWelcomeMessage
              env <- mainLoop initialState
              return ()

printWelcomeMessage = putStrLn $ "Welcome to TeLLeR. " ++ help
help = "Enter a command:\n  (d)ebug mode: on/off, (+) insert resource, (-) remove resource, (l)oad file, (p)rint environment, (s)tart, (r)eset, (q)uit, (h)elp."

createEnvFromFile :: FilePath -> IO ProverState
createEnvFromFile fileName = do
    fileContents <- readFile fileName 
    return (changeEnvTo (tts' fileContents) initialState)








{--
main' = do
     args <- getArgs
     case args of
          [f] -> runFile f
          []  -> runInteractive

runFile f      = readFile f >>= run' term doReductions
runInteractive = getLine    >>= run' term doReductions >> runInteractive
--}

doReductions t = do
  t' <- reduceIO' (map simplify t)
  putStrLn "End of story, no more reductions found for:"
  putStrLn (showTerms t')

reduce'     = findFixpoint   reduce
reduceIO' t = findFixpointIO reduceIO t

reduce   :: [Term] -> [Term]
reduceIO :: [Term] -> IO [Term]

reduce   ts = tryReduction' reduceLolly $ concatMap detensor ts
reduceIO ts = tryReductionsIO reductions (concatMap detensor ts)
         where reductions = [
                   -- reduceOfCourseLollyIO,
                   reduceLollyIO,
                   reduceWithIO,
                   reducePlusIO,
                   reduceOneIO
                 ]

tryReductionsIO :: [IOReduction] -> [Term] -> IO [Term]
tryReductionsIO (f:fs) t = tryReductionIO' f t >>= tryReductionsIO fs
tryReductionsIO []     t = return t

reduceLolly :: Reduction
reduceLolly (a :-@: b, ts)
  | isSimple a = do ts' <- removeProduct' a ts; Just (b:ts')
  | otherwise  = error "lolly LHSs must be simple tensor products"
reduceLolly _  = Nothing

reduceLollyIO :: IOReduction

reduceLollyIO (a :-@: b, ts) =
   removeProductGiving (\ts' -> b:ts') a b ts

reduceLollyIO (t@(OfCourse (a :-@: b)), ts) =
   removeProductGiving (\ts' -> b:t:ts') a b ts

reduceLollyIO _ = return $ Nothing

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

reduceMessage :: Term -> Term -> IO ()
reduceMessage a b = putStrLn $ concat ["reducing: ",   showTerm (a :-@: b),
                                       ", removing: ", showTerm a,
                                       ", adding: ",   showTerm b]

lollyTensorWarning :: IO (Maybe a)
lollyTensorWarning = do 
      putStrLn "warning: lolly LHSs must be simple tensor products"
      return $ Nothing


reduceWithIO :: IOReduction
reduceWithIO (a :&: b, ts) = do t <- choose a b
                                return $ Just (t:ts)
reduceWithIO _ = return Nothing


reducePlusIO :: IOReduction
reducePlusIO (a :+: b, ts) = do t <- chooseRandom a b
                                return $ Just (t:ts)
reducePlusIO _ = return Nothing

reduceOneIO :: IOReduction
reduceOneIO (One, ts) = return $ Just ts
reduceOneIO _ = return Nothing

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
      "Dungeon Master chooses: ", showTerm t', " from ",
      showTerm s, " or ", showTerm t
    ]
  return t'

isSimple ts = all isAtom (detensor ts)

isAtom (Atom _) = True
isAtom _ = False

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


tryReduction'   f ls = fromMaybe ls (tryReduction f ls)
tryReductionIO' f ls = return . fromMaybe ls =<< tryReductionIO f ls

pointedMap f ls = map  f (point ls)

tryReduction   :: Reduction   -> [Term] -> Maybe [Term]
tryReductionIO :: IOReduction -> [Term] -> IO (Maybe [Term])

tryReduction   f ls = msum (pointedMap f ls)
tryReductionIO f ls = go (point ls)
  where go [] = return Nothing
        go (x:xs) = do
            x' <- f x
            case x' of
              Just _  -> return x'
              Nothing -> go xs

point ls = go [] ls
  where go prev (x:next) = (x, prev ++ next) : go (x:prev) next
        go prev [] = []

detensor (a :*: b) = concat [detensor a, detensor b]
detensor x = [x]

pairs (x:y:xs) = (x, y):pairs xs
findRepeat :: Eq a => [a] -> a
findRepeat = fst . fromJust . find (uncurry (==)) . pairs
findFixpoint f = findRepeat . iterate f

findFixpointIO :: Eq a => (a -> IO a) -> a -> IO a
findFixpointIO f x =
  do x' <- f x
     case x' == x of
       True  -> return x'
       False -> findFixpointIO f x'
