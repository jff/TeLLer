module Parser(term, run, run', tt, tts, tts',parse) where

import Syntax

import Text.ParserCombinators.Parsec


-- Grammar Rules --

term :: Parser [Term]
term = do spaces
          terms <- sepEndBy expr (optional (symbol "."))
          eof
          return terms

atom = identifier `as` Atom <?> "atomic formula"

atomish =  atom
       <|> unit
       <|> parens expr

negation = atomish >>= negs

negs x = do
  n <- optionMaybe (symbol "^")
  case n of
    Just _  -> negs (Not x)
    Nothing -> return x


expr  = expr1 `chainr1` imp
expr1 = expr2 `chainr1` add
expr2 = expr3 `chainr1` mul
expr3 = expt


mul    = tensor <|> par
tensor = symbol "*" `construct` (:*:)
par    = symbol "$" `construct` (:$:)


imp    = rlolly <|> (symbol "-"  >> (lolly <|> arrow))
lolly  = symbol "@"  `construct` (:-@:)
arrow  = symbol "!"  `construct` (\l r -> OfCourse (l :-@: r))
rlolly = symbol "@-" `construct` flip (:-@:)


add    = with <|> plus
with   = symbol "&" `construct` (:&:)
plus   = symbol "+" `construct` (:+:)


expt     = ofcourse <|> whynot <|> negation
ofcourse = symbol "!" >> expt `as` OfCourse
whynot   = symbol "?" >> expt `as` WhyNot

unit =  symbol "#"  `giving` Top
    <|> symbol "%"  `giving` Bottom
    <|> symbol "1"  `giving` One
    <|> symbol "0"  `giving` Zero

-- Useful Combinators --

despace p = do x <- p
               spaces
               return x

symbol s   = despace (string s)
identifier = despace $
             do c <- letter
                d <- many (letter <|> digit)
                return (c:d)

parens p = do symbol "("
              inner <- p
              symbol ")"
              return inner


-- Utility Functions --

run' :: Parser a -> (a -> IO ()) -> String -> IO ()
run' p f input
        = case (parse p "<unknown>" input) of
            Left err -> putStr "parse error at " >> print err
            Right x  -> f x

run p input = run' p print input

tts' :: String -> [Term]
tts' s = case parse term "<inline>" s of
         Left err -> error (show err)
         Right  x -> x

tt :: String -> Term
tt s = head (tts' s)

tts = map tt

as :: Parser a -> (a -> b) -> Parser b
as = flip fmap

giving :: Parser b -> a -> Parser a
giving p x = p >> return x

construct :: Parser b -> (a -> a -> a) -> Parser (a -> a -> a)
construct p c = p >> return c
