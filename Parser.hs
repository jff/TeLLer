module Parser where

import Syntax

import Text.ParserCombinators.Parsec

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> putStr "parse error at " >> print err
            Right x  -> print x

as :: Parser a -> (a -> b) -> Parser b
as = flip fmap

construct :: Parser b -> (a -> a -> a) -> Parser (a -> a -> a)
construct p c = p >> return c

despace p = do x <- p
               spaces
               return x

symbol s   = despace (string s)
identifier = despace (many1 letter)

term :: Parser [Term]
term = do spaces
          terms <- sepBy1 expr (optional (symbol "."))
          eof
          return terms

atom = identifier `as` Atom <?> "atomic formula"

parens p = do symbol "("
              inner <- p
              symbol ")"
              return inner

atomish =  atom
       <|> parens expr

expr  = expr1 `chainr1` lolly
expr1 = expr2 `chainr1` add
expr2 = expr3 `chainr1` mul
expr3 = expt

mul    = tensor <|> par
tensor = symbol "*" `construct` (:*:)
par    = symbol "$" `construct` (:$:)

add    = with <|> plus
with   = symbol "&" `construct` (:&:)
plus   = symbol "+" `construct` (:+:)

lolly  = symbol "-@" `construct` (:-@:)

expt     = ofcourse <|> whynot <|> atomish
ofcourse = symbol "!" >> expt `as` OfCourse
whynot   = symbol "?" >> expt `as` WhyNot
