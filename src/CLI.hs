-- | The module 'CLI' defines how to interact with the user in a command-line interface.
--   See also the module 'UserIO'.

module CLI (tellerPrintLn,
            tellerPrint,
            tellerWarning,
            tellerDebug,
            tellerError,
            printListOfActions,
            readStringFromUser,
            readFileNameFromUser,
            askUserForInt,
            choose,
            chooseRandom
           )
where

import System.IO (hFlush, stdout)
import System.Console.Readline (readline, addHistory)
import System.Random

import Syntax (Term)
import Printer (showTerm)


----------------------
-- Output functions --
----------------------
_flushStr :: String -> IO ()
_flushStr s = putStr s >> hFlush stdout

_flushStrLn :: String -> IO ()
_flushStrLn s = putStrLn s >> hFlush stdout


-- | The function 'tellerPrintLn' is used to display messages followed by a newline character.
--   It takes the message as an argument.
tellerPrintLn :: String -> IO ()
tellerPrintLn = _flushStrLn

-- | The function 'tellerPrint' is used to display messages. It is similar to 'tellerPrintLn',
--   but it does not append a newline character to the message.
--   It takes the message as an argument.
tellerPrint :: String -> IO ()
tellerPrint = _flushStr

-- | The function 'tellerWarning' is used to display warnings.
--   It takes the warning message as an argument.
tellerWarning :: String -> IO ()
tellerWarning = _flushStrLn

-- | The function 'tellerError' is used to display errors.
--   It takes the error message as an argument.
tellerError :: String -> IO ()
tellerError = _flushStrLn



-- | The function 'tellerDebug' is used to display debug statements.
--   It takes the debug statement as an argument.
--   As a rule, these should only be shown when debug mode is on.
tellerDebug :: String -> IO ()
tellerDebug = _flushStrLn

printListOfActions l = do
    _flushStrLn "There are several actions available. Please choose one of the following:"
    let f = \(n,a) -> _flushStr ((show n) ++ ") ") >> _flushStrLn (show a)
    sequence_ $ map f (zip [0..] l)
 

----------------------
-- Input functions --
----------------------
-- ask user for string
-- TODO: Improve reliability; tabs/auto-complete (readline), etc.
readStringFromUser :: String -> IO String
readStringFromUser query = do
  _flushStr query
  answer <- readline ""
  case answer of
    Nothing -> return ""
    Just line -> return line

readFileNameFromUser :: String -> IO FilePath
readFileNameFromUser query = do
    f <- readStringFromUser "Load file: "
    return ((head.words) f) -- return the first word

askUserForInt :: (Int->Bool) -> String -> IO Int
askUserForInt validate query = do
    _flushStr query
    answer <- getLine
    let n = reads answer :: [(Int, String)]
    case n of
        []      -> _flushStrLn "Not a number. Please try again!" >> askUserForInt validate query
        (i,_):_ -> if (validate i) then return i
                                   else _flushStrLn "Invalid number. Please try again!" >> askUserForInt validate query
    




askUserIfProceed :: IO Bool
askUserIfProceed = do
    _flushStrLn "Do you want to proceed? (y/n)"
    answer <- getLine
    case answer of
        ('y':_) -> return True
        ('n':_) -> return False
        _       -> (_flushStrLn "Invalid choice! Try again.") >> askUserIfProceed
    



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



