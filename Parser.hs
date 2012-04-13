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

symbol s = string s

term :: Parser Term
term = expr

atom = many1 letter `as` Atom <?> "atomic formula"
negation = symbol "~" >> term `as` Not

parens p = do symbol "("
              inner <- p
              symbol ")"
              return inner

atomish =  parens expr
       <|> atom
       <|> negation

expr  = expr1 `chainr1` lolly
expr1 = expr2 `chainr1` add
expr2 = expr3 `chainr1` mul
expr3 = atomish

mul    = tensor <|> par
tensor = symbol "*" `construct` (:*:)
par    = symbol "$" `construct` (:$:)

add    = with <|> plus
with   = symbol "&" `construct` (:&:)
plus   = symbol "+" `construct` (:+:)

lolly  = symbol "-@"  `construct` (:-@:)
