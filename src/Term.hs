module Term where

import Syntax
--import Control.Applicative 
import Data.List (intersect, (\\))


isSimple ts = all isAtom (detensor ts)

isAtom (Atom _) = True
isAtom _ = False



detensor (a :*: b) = concat [detensor a, detensor b]
detensor x = [x]

listEnabledActions :: [Term] -> [Term]
--listEnabledActions env = filter (isEnabledAction (concatMap detensor env)) env
listEnabledActions env = listEnabledActionsBy env env

listEnabledActionsBy :: [Term] -> [Term] -> [Term]
listEnabledActionsBy terms env = filter (isEnabledAction (concatMap detensor terms)) env

-- TODO, FIXME: Improve efficiency
isEnabledAction :: [Term] -> Term -> Bool
isEnabledAction env (t1 :-@: t2) = let resources = linearizeTensorProducts [t1] --and $  elem <$> (linearizeTensorProducts [t1]) <*> [env]
                                       linearEnv = linearizeTensorProducts env
                                       resourcesAvailable = (resources \\ (intersect linearEnv resources)) == []
                                   in  (isSimple t1) && resourcesAvailable
isEnabledAction env (t1 :&: t2)  = isEnabledAction env t1 && isEnabledAction env t2
isEnabledAction env (t1 :+: t2)  = isEnabledAction env t1 || isEnabledAction env t2
isEnabledAction env (OfCourse (t1 :-@: t2))  = isEnabledAction env (t1 :-@: t2)
isEnabledAction _ _              = False

linearizeTensorProducts :: [Term] -> [Term]
linearizeTensorProducts = concatMap detensor 
