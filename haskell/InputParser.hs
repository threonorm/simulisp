module InputParser (inputParser) where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.List
import qualified Data.Map as Map

import Text.Parsec hiding (token)
import Text.Parsec.String

import NetlistAST

 
oneInput :: Parser (Ident,Value)
oneInput = do
  many $ char ' ' 
  first <- letter <|> char '_'
  after <- many $ letter <|> digit <|> char '_'
  many $ char ' ' 
  char ':'
  many $ char ' ' 
  value <- many $ (many $ char ' ') *> oneOf ['0','1'] <*(many $ char ' ')
  return (first:after,convert value)
  where convert [t] = VBit $ t/= '0'
        convert cs = VBitArray $ map (/= '0') cs
          

lineInput :: Parser [(Ident,Value)]
lineInput = do
  list<-(:) <$> oneInput <*> many (char ',' *> oneInput)
  return list 

inputParser :: Parser [[(Ident,Value)]]
inputParser = do
  allSteps <- (:) <$> lineInput <*> many (try $ (many newline *> lineInput)) 
  spaces
  eof
  return allSteps     
