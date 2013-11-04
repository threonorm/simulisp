module InputParser (inputParser, inlineInputParser) where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.List
import qualified Data.Map as Map

import Text.Parsec hiding (token)
import Text.Parsec.String

import NetlistAST


-- Parser for input files
inputParser :: Parser [[(Ident,Value)]]
inputParser = do
  -- commentaire pour Thomas : on aurait du utiliser lineInput `sepBy1` newline
  -- (en fouillant dans Text.Parsec.Combinator on trouve des trucs très pratiques)
  allSteps <- (:) <$> lineInput <*> many (try $ (many newline *> lineInput)) 
  eof
  return allSteps     

lineInput :: Parser [(Ident,Value)]
lineInput = do
  list<-(:) <$> oneInput <*> many (char ',' *> oneInput)
  return list 

-- similar to the "spaces" parser, but doesn't recognize newlines
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
