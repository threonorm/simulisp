module Lisp.MiniLispParser (miniLispParser,
                            LispProgram,
                            LispDefun(..),
                            LispExpr(..),
                            LispIdent,
                            LLiteral(..)) where

import Prelude hiding (lex)

import Control.Monad
import Control.Exception (assert)
import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map as Map

import Text.Parsec hiding (token, (<|>), many)
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

import Lisp.Primitives

type LispProgram = [LispDefun]

data LispDefun = LispDefun LispIdent [LispIdent] [LispExpr]
               deriving (Show)

data LispExpr = LNil
              | LNum Int
              | LVar LispIdent -- includes primitives
              | LApp LispExpr [LispExpr]
              -- hardcoding special operators directly...
              | LQuoteList LLiteral -- support only a special case of quote               
              | LIf LispExpr LispExpr LispExpr
              | LLambda [LispIdent] [LispExpr]
              | LLet [(LispIdent, LispExpr)] [LispExpr]
              | LBegin [LispExpr]
              | LSync  [LispExpr]
              deriving (Show)

data LLiteral = LLitNum Int
              | LLitNil
              | LLitList [LLiteral]
              deriving (Show)

type LispIdent = String

miniLispParser :: Parser LispProgram
miniLispParser = whiteSpace lex *> program

-- automatically generated lexer
lex :: TokenParser st
lex = makeTokenParser langDef
  where langDef = emptyDef { commentLine = ";"
                           , identStart = letter <|> oneOf "+-*/<>=?!"
                           , identLetter = identStart langDef <|> digit
                           , opStart = parserZero -- no infix operators in Lisp!
                           , opLetter = parserZero
                           , reservedNames = ["defun"]
                           }

listOf p = parens lex (many p) 

program = many defun

defun = parens lex $ symbol lex "defun" *>
        (LispDefun <$> identifier lex
                   <*> listOf (identifier lex)
                   <*> many expr)

expr = choice $ map try [ LNil <$ symbol lex "()"
                        , LNum . fromInteger <$> natural lex
                        , LVar <$> identifier lex
                        , symbol lex "\'" *> (LQuoteList <$> quotedList)
                        ]
                ++ [ parens lex composite ]

composite = (do header <- try $ identifier lex
                operation header)
            <|> (LApp <$> expr <*> many expr)
            
operation "if"          = LIf     <$> expr <*> expr <*> expr
operation "lambda"      = LLambda <$> listOf (identifier lex) <*> many expr
operation "let"         = LLet    <$> letBindings <*> many expr
operation "begin"       = LBegin  <$> many expr
operation "synchronize" = LSync   <$> many expr
operation ident         = LApp (LVar ident) <$> many expr

letBindings = parens lex $ many binding
  where binding = parens lex $ (,) <$> identifier lex <*> expr


quotedList = LLitList <$> listOf quotedExpr
  where quotedExpr = choice [ LLitNil <$ try (symbol lex "()")
                            , LLitNum . fromInteger <$> try (natural lex)
                            , quotedList ]


