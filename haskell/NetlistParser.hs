{-# LANGUAGE TupleSections #-}

module NetlistParser (netlistParser) where

import Control.Monad
import Control.Applicative hiding ((<|>), choice, many)
import Data.List

import Text.Parsec hiding (token)
import Text.Parsec.Char
import Text.Parsec.String

import NetlistAST

netlistParser :: Parser Program
netlistParser = spaces *> netlist <* eof

token = (<* spaces)

identStartChar = letter <|> char '_'
identChar = identStartChar <|> digit <|> char '\''
keyword x = token . try $ string x *> notFollowedBy identChar
punctuation = token . char

netlist = Pr <$> inputs <*> outputs <*> vars
             <*> (keyword "IN" equations

ident = token ( (:) <$> identStartChar <*> many identChar )


bigList header eltParser = keyword header
                           *> eltParser `sepBy` (punctuation ',')

inputs = bigList "INPUT" ident
outputs = bigList "OUTPUT" ident
vars = bigList "VAR" var
  where var = do x <- ident
                 n_ <- optionMaybe (punctuation ':' *> many1 digit)
                 return . (x,) $ case n_ of
                   Nothing -> TBit
                   Some s -> TBitArray (read s)

equations = many equation

equation = do z <- ident
              punctuation '='
              e <- exp
              return (z, e)

-- every choice is determined by the heading keyword except arg
--   --> put arg at the end
exp = choice [ k "NOT" $ Enot <$> arg
             , k "REG" $ Ereg <$> ident
             , undefined
             , binop
             , arg
             ]
  where
    k x p = keyword x *> p
    

  
