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

main = do
     args <- getArgs
     case args of
          [f] -> runFile f
          []  -> runInteractive

runFile f = readFile f >>= run' term doReductions
runInteractive = getLine >>= run' term doReductions >> main

doReductions t = do
  t' <- reduceIO' (map simplify t)
  putStrLn "End of story, no more reductions found for:"
  putStrLn (showTerms t')

reduce'     = findFixpoint   reduce
reduceIO' t = findFixpointIO reduceIO t

reduce   :: [Term] -> [Term]
reduceIO :: [Term] -> IO [Term]

reduce   ts = tryReduction' reduceLolly $ concatMap detensor ts
reduceIO ts = return (concatMap detensor ts)
          >>= tryReductionIO' reduceOfCourseLollyIO
          >>= tryReductionIO' reduceLollyIO
          >>= tryReductionIO' reduceWithIO
          >>= tryReductionIO' reducePlusIO
          >>= tryReductionIO' reduceOneIO

reduceLolly :: (Term, [Term]) -> Maybe [Term]
reduceLolly (a :-@: b, ts)
  | simplep a = do ts' <- removeProduct' a ts; Just (b:ts')
  | otherwise = error "lolly LHSs must be simple tensor products"
reduceLolly _ = Nothing

reduceLollyIO :: (Term, [Term]) -> IO (Maybe [Term])
reduceLollyIO (a :-@: b, ts)
  | simplep a = case removeProduct' a ts of
                     Nothing   -> return Nothing
                     Just ts'  -> do
                       putStrLn $ concat ["reducing: ",   showTerm (a :-@: b),
                                          ", removing: ", showTerm a,
                                          ", adding: ",   showTerm b]
                       return $ Just (b:ts')

  | otherwise = do putStrLn "warning: lolly LHSs must be simple tensor products"
                   return $ Nothing

reduceLollyIO _ = return $ Nothing


reduceWithIO :: (Term, [Term]) -> IO (Maybe [Term])
reduceWithIO (a :&: b, ts) = do t <- choose a b
                                return $ Just (t:ts)
reduceWithIO _ = return Nothing


reducePlusIO :: (Term, [Term]) -> IO (Maybe [Term])
reducePlusIO (a :+: b, ts) = do t <- chooseRandom a b
                                return $ Just (t:ts)
reducePlusIO _ = return Nothing

reduceOneIO :: (Term, [Term]) -> IO (Maybe [Term])
reduceOneIO (One, ts) = return $ Just ts
reduceOneIO _ = return Nothing

reduceOfCourseLollyIO :: (Term, [Term]) -> IO (Maybe [Term])
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

simplep ts = all atomp (detensor ts)

atomp (Atom _) = True
atomp _ = False

removeProduct'  :: Term -> [Term] -> Maybe [Term]
removeProduct   :: [String] -> [String] -> Maybe [String]

removeProduct' t ts =
  let deatom (Atom s) = s
      used = map deatom (detensor t)
      (atoms, rest) = partition atomp ts
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


tryReduction :: ((Term, [Term]) -> Maybe [Term]) -> [Term] -> Maybe [Term]
tryReductionIO ::
  ((Term, [Term]) -> IO (Maybe [Term])) -> [Term] -> IO (Maybe [Term])

tryReduction'   f ls = fromMaybe ls (tryReduction f ls)
tryReductionIO' f ls = return . fromMaybe ls =<< tryReductionIO f ls

pointedMap f ls = map  f (point ls)

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
