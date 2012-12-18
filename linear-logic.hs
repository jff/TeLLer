module Main where

import Data.List
import Data.Maybe
import Control.Monad
import System.Random
import System.Environment (getArgs)

import Bag
import Syntax
import Parser
import Printer
import RewriteRules

type Reduction   = (Term, [Term]) -> Maybe [Term]
type IOReduction = (Term, [Term]) -> IO (Maybe [Term])

main = do
     args <- getArgs
     case args of
          [f] -> runFile f
          []  -> runInteractive

runFile f      = readFile f >>= run' term doReductions
runInteractive = getLine    >>= run' term doReductions >> runInteractive

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
