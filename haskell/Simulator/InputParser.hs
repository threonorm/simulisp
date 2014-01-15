module Simulator.InputParser (inputParser, inlineInputParser, romParser) where

import Control.Applicative hiding ((<|>), many)

import Text.Parsec hiding (token)
import Text.Parsec.String

import Netlist.AST


-- Parser for input files
inputParser :: Parser [[(Ident,Value)]]
inputParser = do
  allSteps <- (:) <$> lineInput <*> many (try $ (many newline *> lineInput)) 
  spaces
  eof
  return allSteps     

lineInput :: Parser [(Ident,Value)]
lineInput = do
  list<-(:) <$> oneInput <*> many (char ',' *> oneInput)
  return list 

-- similar to the "spaces" parser, but doesn't recognize newlines
espaces :: Parser String
espaces = many $ char ' '

oneInput :: Parser (Ident,Value)
oneInput = do
  espaces 
  first <- letter <|> char '_'
  after <- many $ letter <|> digit <|> char '_'
  espaces 
  char ':'
  espaces 
  value <- many $ espaces *> oneOf ['0','1'] <* espaces
  return (first:after,convert value)

convert :: String -> Value
convert [t] = VBit $ t /= '0'
convert cs = VBitArray $ map (/= '0') cs

-- Parser for input delivered via the command line
-- This is a variant of lineInput
inlineInputParser :: Parser [(Ident,Value)]
inlineInputParser = ( (,) <$> identifier <*> (char ':' *> value) )
                    `sepBy` (char ',')
  where identifier = (:) <$> (letter <|> char '_')
                         <*> (many $ letter <|> digit <|> char '_')
        value = convert <$> many1 (oneOf ['0', '1'])

-- Parser for ROM files

romParser :: Parser [(Ident, [Bool])]
romParser = (romLine `sepBy` char 'n') <* (spaces >> eof)

romLine :: Parser (Ident, [Bool])
romLine = (,) <$> ((:) <$> (letter <|> char '_')
                       <*> (many $ letter <|> digit <|> char '_'))
              <*> (char ':' *> (map (/='0') <$> many (oneOf ['0','1'])))

