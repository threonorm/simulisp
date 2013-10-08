module NetlistParser (netlistParser) where

import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding (token)
import Text.Parsec.Char
import Text.Parsec.String

import NetlistAST

netlistParser :: Parser Program
netlistParser = spaces *> netlist

token = (<* spaces)

identStartChar = letter <|> char '_'
identChar = identStartChar <|> digit <|> char '\''
keyword x = token . try $ string x *> notFollowedBy identChar
punctuation = token . char

netlist = Pr <$> inputs <*> outputs <*> vars <*> equations

ident = token ( (:) <$> identStartChar <*> many identChar )


bigList header eltParser = keyword header
                           *> eltParser `sepBy` (punctuation ',')

inputs = bigList "INPUT" ident
outputs = bigList "OUTPUT" ident
vars = bigList "VAR" var
  where var = do
          x <- ident
          blabla -- gérer bitarray
          

