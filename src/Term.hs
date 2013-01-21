module Term where

import Syntax
import Control.Applicative 


detensor (a :*: b) = concat [detensor a, detensor b]
detensor x = [x]

listEnabledActions :: [Term] -> [Term]
--listEnabledActions env = filter (isEnabledAction (concatMap detensor env)) env
listEnabledActions env = listEnabledActionsBy env env

listEnabledActionsBy :: [Term] -> [Term] -> [Term]
listEnabledActionsBy terms env = filter (isEnabledAction (concatMap detensor terms)) env

isEnabledAction :: [Term] -> Term -> Bool
isEnabledAction env (t1 :-@: t2) = and $  elem <$> (linearizeTensorProducts [t1]) <*> [env]
isEnabledAction env (t1 :&: t2)  = isEnabledAction env t1 && isEnabledAction env t2
isEnabledAction _ _              = False

linearizeTensorProducts :: [Term] -> [Term]
linearizeTensorProducts = concatMap detensor 
