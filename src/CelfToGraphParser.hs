--
-- A parser for Celf queries output. Possibly, the worst parser ever written...
--
module CelfToGraphParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Syntax
import Parser (tts')
import qualified Data.Map as Map 

type Var = String
type RuleName = String
data CelfOutput = Celf [Term] (Map.Map String Term) [Solution] deriving (Show)
data Solution   = Lambda Var [LetBinding] deriving (Show)
data LetBinding = Let [Var] (Maybe RuleName) [Var] deriving (Show)

-- generated manually
x :: CelfOutput
x = Celf (tts' "a*b")
         (Map.fromList [("a1",head $ tts' "a-@b*b"),("a2",head $ tts' "b*b-@c")])
         [(Lambda "X1" [ Let ["X2","X3"] Nothing     ["X1"],
                        Let ["X4","X5"] (Just "a1") ["X2"],
                        Let ["X6"]      (Just "a2") ["X5","X3"]
                      ])
         ]

y :: CelfOutput
y =  Celf (tts' "k * l * p")
          (Map.fromList [("o1", head $ tts' "k-@m"),
                         ("o2", head $ tts' "l-@m"),
                         ("a1", head $ tts' "l-@d"),
                         ("a2", head $ tts' "p-@m*m"),
                         ("a3", head $ tts' "m*m-@f")
                        ])
          [(Lambda "X1" [
                            Let ["X2","X3","X4"] Nothing ["X1"],
                            Let ["X5","X6"] (Just "a2") ["X4"],
                            Let ["X7"] (Just "o2") ["X3"],
                            Let ["X8"] (Just "a3") ["X5","X6"],
                            Let ["X9"] (Just "o1") ["X2"],
                           Let ["X10"] (Just "a3") ["X9","X7"]
                       ])
          ]

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = [ "Solution:"
                                     , "let"
                                     , "in"
                                     , "type"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                     , "<", ">", "and", "or", "not"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
symbol     = Token.symbol lexer -- parses whitespace

celfOutputParser :: Parser CelfOutput
celfOutputParser = do
    whiteSpace
    parseOpeningInfo
    sepEndBy (try parseType) (symbol "\n") -- parse types, whilst possible
    actions <- sepEndBy (try parseAction) (symbol "\n")
    init <- parseInit
    solutions <- parseSolutions
    return (Celf init (Map.fromList actions) solutions)

parseOpeningInfo = do
    manyTill anyChar (try (string "]\n"))
    
parseType = do
    identifier
    string ": type."

parseAction :: Parser (String,Term)
parseAction = do
    name <- identifier
    string ": "
    -- I only want to parse actions, so the lollipop has to be present
    term1 <- manyTill anyChar (try (string "-o "))
    term2 <- manyTill anyChar (try (string "."))
    return (name, head $ tts' (celf2teller (term1++"-o "++term2)))

celf2teller :: String -> String
celf2teller s = 
    if (take 7 s == "Type = ") then strip (drop 7 s)-- init
                               else (convertLolli . strip) s
    where strip = filter (not . \x -> x `elem` ['{','}','@','!'])
          convertLolli (x:y:xs) | (x=='-') && (y=='o') = '-':'@':convertLolli xs
                                | otherwise = x:convertLolli (y:xs)
          convertLolli (x:xs) = x:convertLolli xs
          convertLolli [] = []
    

parseInit :: Parser [Term]
parseInit = do
    string "init: Type ="
    term <- manyTill anyChar (try (string ".\n"))
    return (tts' (celf2teller $ term))

bracketList = ['(',')','{','}','[',']']
removeBrackets = filter (not . (\c -> c `elem` bracketList)) 

parseSolutions :: Parser [Solution]
parseSolutions = do
    sepEndBy (try parseNoSolution <|> try parseSolution) (symbol "\n")

parseNoSolution :: Parser Solution
parseNoSolution = do
    optional (string "Iteration " >> integer)
    string "No solutions found."
    return $ Lambda "" []

parseSolution :: Parser Solution
parseSolution = do
    optional (string "Iteration " >> integer)
    string "Solution: \\"
    optional (symbol "@")
    v <- identifier
    string "."
    whiteSpace
    lbs <- between (string "{\n") (string "}" >> lookAhead (string"\n")) parseLetBinding
    return (Lambda v lbs)

parseLetBinding = do
    --sepEndBy (try parseLet) (lookAhead (symbol "}") <|> (symbol "\n"))
    sepEndBy (try parseLet) (symbol "\n")

parseLet :: Parser LetBinding
parseLet = do
    whiteSpace
    string "let"
    whiteSpace
    lvleft <- between (symbol "{") (symbol "}") parsePossiblyNestedLists
    whiteSpace
    symbol "="
    whiteSpace
 --   manyTill anyChar (try (string "in" >> whiteSpace >> identifier) <|> try (string "in") )
    (actionName,lvright) <- (try p2 <|> try p1)
    return $ Let lvleft actionName lvright
--    return $ Let lvleft Nothing []

p1 = do
    lvright <- parsePossiblyNestedLists
    whiteSpace
    manyTill anyChar (try (string "in" >> whiteSpace >> identifier >> lookAhead (symbol "}")) <|> try (string "in ") )
    return (Nothing, lvright)

p2 = do
    ruleName <- identifier
    whiteSpace
    lvright <- parsePossiblyNestedLists
    whiteSpace
    manyTill anyChar (try (string "in" >> whiteSpace >> identifier >> lookAhead (symbol "}")) <|> try (string "in ") )
    return (Just ruleName, lvright)

parsePossiblyNestedLists :: Parser [Var]
parsePossiblyNestedLists = do
    s <- try p3 <|> try ((symbol "@" <|> symbol "!") >> identifier) <|> try identifier  <|> try (string "1")
    return $ wordsWhen (==',') $ strip $ removeBrackets s

    where strip = filter (not . (\c -> c `elem` ['@',' ', '!']))

p3 = do
    symbol "["
    l <- sepBy cell (symbol ",")
    symbol "]"
    return $ drop 1 $ concatMap (',':) l

cell = try p3 <|> (try ((symbol "@" <|> symbol "!") >> identifier)) <|> try hashNumber
hashNumber = do
    v <- identifier
    optional (symbol "#" >> integer)
    return $ v
    
-- Auxiliary methods
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

