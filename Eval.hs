{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Eval where
import Expression
import Control.Applicative
import qualified Data.Map as M
import Data.Text (Text)
import Data.Functor.Identity (Identity )
import Control.Monad.Reader

type Reader' = Reader Context 

data Context = Context {
      fields :: [Text] -- the DSV fields
    , trueStrings :: [Text]
    , falseStrings :: [Text]
    , nullStrings :: [Text]
    } deriving Show

exprEvalToString :: Expr -> Reader' String
exprEvalToString e = undefined

evalToBool :: Expr -> Reader' Bool
evalToBool e =
    let val = exprEval expr context
    in valueToBool val

-- handle all the cases
exprEval :: Expr -> Context -> 
exprEval (FieldNum n) =
exprEval (And x y) =
exprEval (Or x y) =
exprEval (Compare op x y) =
exprEval (StringChoice map) =
exprEval (LiteralExpr x) =
