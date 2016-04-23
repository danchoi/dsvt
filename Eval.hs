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

{-
exprEvalToString :: Expr -> Reader' String
exprEvalToString e = undefined

evalToBool :: Expr -> Reader' Bool
evalToBool e =
    let val = exprEval expr context
    in valueToBool val
-}


exprEvalToString :: Expr -> Context -> String
exprEvalToString (FieldNum n) Context{..} = undefined
exprEvalToString (And x y) Context{..} = undefined
exprEvalToString (Or x y) Context{..} = undefined
exprEvalToString (Compare op x y) Context{..} = undefined
exprEvalToString (StringChoice map) Context{..} = undefined
exprEvalToString (LiteralExpr x) Context{..} = undefined

exprEvalToBool :: Expr -> Context -> Bool
exprEvalToBool (FieldNum n) Context{..} = undefined
exprEvalToBool (And x y) Context{..} = undefined
exprEvalToBool (Or x y) Context{..} = undefined
exprEvalToBool (Compare op x y) Context{..} = undefined
exprEvalToBool (StringChoice map) Context{..} = undefined
exprEvalToBool (LiteralExpr x) Context{..} = undefined

