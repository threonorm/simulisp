{-# LANGUAGE TupleSections, ParallelListComp, PatternGuards #-}

module Lisp.MiniLispCompiler where

-- Stupid compiler which doesn't even check, for example,
-- that functions get the right number of arguments
-- Made straightforward by the convenient target instruction set

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

import Lisp.MiniLispParser
import Lisp.SCode
import Lisp.Primitives

compileProgram :: LispProgram -> Maybe SProgram
compileProgram prog = mapM f prog
  where f (LispDefun name args body) =
          (name,) <$> compileFunction globals args body
        globals = map (\(LispDefun name _ _) -> name) prog

data Variable = LocalVar Int Int | GlobalVar | PrimVar Primitive
type VariableMap = Map LispIdent Variable

primMap :: VariableMap
primMap = Map.fromList . map (second PrimVar) $
          [ ("car",   PCar)
          , ("cdr",   PCdr)
          , ("cons",  PCons)
          , ("+1",    PIncr)
          , ("-1",    PDecr)
          , ("=0?",   PIsZero)
          , (">=60?", PIsgt60)
          , (">=24?", PIsgt24)
          , ("print-second", PPrintSec)
          , ("print-minute", PPrintMin)
          , ("print-hour",   PPrintHour)
          ]

compileFunction :: [LispIdent] -> [LispIdent] -> [LispExpr] -> Maybe SWord
compileFunction globals args body = compileSequence varmap body
  where varmap = localMap `Map.union` globalMap `Map.union` primMap
        -- remember: the union is left-biased, i.e. arguments shadow global vars
        localMap  = makeArgsMap args
        globalMap = Map.fromList $ zip globals (repeat GlobalVar)

makeArgsMap :: [LispIdent] -> VariableMap
makeArgsMap args = Map.fromList [ (x, LocalVar 0 i) | x <- args
                                                    | i <- [0..] ]

compileSequence :: VariableMap -> [LispExpr] -> Maybe SWord
compileSequence varmap = foldr1 f . map (compileExpr varmap)
  where f (Just x) (Just y) = Just $ SWord TSequence (SPtr (x, y))
        f _        _        = Nothing


compileExpr :: VariableMap -> LispExpr -> Maybe SWord

compileExpr _ LNil     = Just snil
compileExpr _ (LNum n) = Just $ SWord TNum (SNum n)
compileExpr varmap (LVar ident) = do
  v <- Map.lookup ident varmap
  return $ case v of
    LocalVar n k -> SWord TLocal  (SLocal n k)
    GlobalVar    -> SWord TGlobal (SGlobal ident)
    PrimVar prim -> SWord (TPrim prim) svoid

compileExpr varmap (LApp fn args) =
  join $ appli <$> compileExpr varmap fn <*> mapM (compileExpr varmap) args
  where appli _   []     = Nothing -- currently, there's no support for 0 args
        appli fn' [arg'] = Just $ SWord TApplyOne (SPtr (arg', fn'))
        appli fn' (arg':args') =
          Just $ f (SWord TLast (SPtr (arg', fn'))) args'
          where f acc [x]    = SWord TFirst (SPtr (x, acc))
                f acc (x:xs) = f (SWord TNext (SPtr (x, acc))) xs

compileExpr _ (LQuoteList quotedList) = Just $ compileQuote quotedList

compileExpr varmap (LIf predicate consequent alternative) =
  f <$> compileExpr varmap predicate
    <*> compileExpr varmap consequent
    <*> compileExpr varmap alternative
  where f p c a = SWord TSequence (SPtr (p,
                                         SWord TCond (SPtr (c, a))))
        
compileExpr varmap (LLambda args body) =
  compileSequence newVarmap body
  where newVarmap = makeArgsMap args `Map.union` deepen varmap

compileExpr varmap (LLet [] body) = compileSequence varmap body
compileExpr varmap (LLet ((ident, expr):otherBindings) body) = do
  expr' <- compileExpr varmap expr
  body' <- compileExpr newVarmap (LLet otherBindings body)
  return $ SWord TLet (SPtr (expr', body'))
  where newVarmap = Map.singleton ident (LocalVar 0 0)
                    `Map.union` deepen varmap
        
compileExpr varmap (LBegin body) = compileSequence varmap body

compileExpr varmap (LSync body) =
  SWord TSync . SPtr . (,snil) <$> compileSequence varmap body

deepen :: VariableMap -> VariableMap
deepen = fmap f
  where f (LocalVar n k) = LocalVar (n+1) k
        f x = x


compileQuote :: LLiteral -> SWord
compileQuote LLitNil      = snil
compileQuote (LLitNum n)  = SWord TNum (SNum n)
compileQuote (LLitList l) = foldr1 f . map compileQuote $ l
  where f x y = SWord TList (SPtr (x, y))

