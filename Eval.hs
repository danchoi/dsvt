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
      fields :: [Text] -- the DSV field values, as text
    , trueStrings :: [Text]
    , falseStrings :: [Text]
    , nullStrings :: [Text]
    } deriving Show

{-
exprEvalToString :: Expr -> Reader' String
exprEvalToString e = undefined

evalToBool :: Expr -> Reader' Bool
evalToBool e =
    let val = exprEval expr context
    in valueToBool val
-}


exprEvalToString :: Expr -> Reader' String 
exprEvalToString (FieldNum n) = undefined
exprEvalToString (And x y) = undefined
exprEvalToString (Or x y) = undefined
exprEvalToString (Compare op x y) = undefined
exprEvalToString (StringChoice map) = undefined
exprEvalToString (LiteralExpr x) = undefined

exprEvalToBool :: Expr -> Reader' Bool
exprEvalToBool (FieldNum n) = undefined
exprEvalToBool (And x y) = undefined
exprEvalToBool (Or x y) = undefined
exprEvalToBool (Compare op x y) = undefined
exprEvalToBool (StringChoice map) = undefined
exprEvalToBool (LiteralExpr x) = undefined

litToBool :: Literal -> Bool
litToBool (LitString "") = False
litToBool (LitBool False) = False
litToBool LitNull = False
litToBool _ = True
