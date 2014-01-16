{-# LANGUAGE TupleSections #-}

module Netlist.Parser (netlistParser) where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.List
import qualified Data.Map as Map

import Text.Parsec hiding (token)
import Text.Parsec.String

import Netlist.AST

netlistParser :: Parser Program
netlistParser = spaces *> netlist <* eof

token t = t <* spaces

identStartChar = letter <|> char '_'
identChar = identStartChar <|> digit <|> char '\''
keyword x = token . try $ string x *> notFollowedBy identChar
punctuation = token . char

netlist = Pr <$> inputs <*> outputs <*> vars
             <*> (keyword "IN" *> equations)

ident = try $ do
  foo <- token ( (:) <$> identStartChar <*> many identChar )
  if foo `elem` ["INPUT", "OUTPUT", "VAR", "IN"]
    then mzero
    else return foo


bigList header eltParser = keyword header
                           *> eltParser `sepBy` (punctuation ',')

inputs = bigList "INPUT" ident
outputs = bigList "OUTPUT" ident
vars = Map.fromList <$> bigList "VAR" var
  where var = do x <- ident
                 n_ <- optionMaybe (punctuation ':' *> many1 digit <* spaces)
                 return . (x,) $ case n_ of
                   Nothing -> TBit
                   Just s -> TBitArray (read s)

equations = many equation

equation = do z <- ident
              punctuation '='
              e <- expr
              return (z, e)

-- every choice is determined by the heading keyword except arg
--   --> put arg at the end
expr = choice [ k "NOT" $ Enot <$> arg
              , k "REG" $ Ereg <$> ident
              , k "MUX" $ Emux <$> arg <*> arg <*> arg
              , k "ROM" $ Erom <$> int <*> int <*> arg
              , k "RAM" $ Eram <$> int <*> int <*> arg <*> arg <*> arg <*> arg
              , k "CONCAT" $ Econcat <$> arg <*> arg
              , k "SELECT" $ Eselect <$> int <*> arg
              , k "SLICE" $ Eslice <$> int <*> int <*> arg
              , binop
              , Earg <$> arg
              ]
  where
    binop = choice . map (\(name, op) -> k name $ Ebinop op <$> arg <*> arg)
            $ [("AND", And), ("OR", Or), ("NAND", Nand), ("XOR", Xor)]
    k x p = keyword x *> p
    int = foldl' (\x y -> 10*x + digitToInt y) 0 <$> token (many1 digit)
    arg = try (Aconst <$> const') <|> Avar <$> ident
    const' = to_const <$> token (many1 bit)
    bit =     (False <$ char '0') <|> (True <$ char '1')
          <|> (False <$ char 'f') <|> (True <$ char 't')
    to_const [] = assert False undefined
    to_const [b] = VBit b
    to_const bs = VBitArray bs
    
  
