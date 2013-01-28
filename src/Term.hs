module Term where

import Syntax
--import Control.Applicative 
import Data.List (intersect, (\\))


isSimple ts = all isAtomicResource (detensor ts)

isAtom (Atom _) = True
isAtom _ = False

isPersistentAtom (OfCourse (Atom _)) = True
isPersistentAtom _ = False

-- | isAtomicResource is used to determine if a resource is an atom or a persistent atom
isAtomicResource a = isAtom a || isPersistentAtom a


detensor (a :*: b) = concat [detensor a, detensor b]
detensor x = [x]

listEnabledActions :: [Term] -> [Term]
--listEnabledActions env = filter (isEnabledAction (concatMap detensor env)) env
listEnabledActions env = listEnabledActionsBy env env

listEnabledActionsBy :: [Term] -> [Term] -> [Term]
listEnabledActionsBy terms env = filter (isEnabledAction (concatMap detensor terms)) env

-- TODO, FIXME: Improve efficiency
isEnabledAction :: [Term] -> Term -> Bool
isEnabledAction env ((:-@:) t1 t2 _) = 
    let need = linearizeTensorProducts [t1] --and $  elem <$> (linearizeTensorProducts [t1]) <*> [env]
        available = linearizeTensorProducts env
        int = intersect available need
        intPersistent = intersect available (map OfCourse need) -- check for persistent resources
        simpleNotAvailable = need \\ int
        persistentNeeded = map OfCourse simpleNotAvailable
        nonExistent = persistentNeeded \\ (concat . replicate (length persistentNeeded)) intPersistent -- nonExistent = map ! (need\\int)\\intPersistent
        --resourcesAvailable = (need \\ (intersect available need)) == []
        resourcesAvailable = nonExistent == []
    in  (isSimple t1) && resourcesAvailable
isEnabledAction env (t1 :&: t2)  = isEnabledAction env t1 && isEnabledAction env t2
isEnabledAction env (t1 :+: t2)  = isEnabledAction env t1 || isEnabledAction env t2
isEnabledAction env (OfCourse ((:-@:) t1 t2 d))  = isEnabledAction env ((:-@:) t1 t2 d)
isEnabledAction _ _              = False

linearizeTensorProducts :: [Term] -> [Term]
linearizeTensorProducts = concatMap detensor 
