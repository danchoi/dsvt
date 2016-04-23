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
exprEvalToString x@(And _ _) = show <$> exprEvalToBool x
exprEvalToString x@(Or _ _) = show <$> exprEvalToBool x
exprEvalToString x@(Compare _ _ _) = show <$> exprEvalToBool x
exprEvalToString (StringChoice map) = undefined
exprEvalToString (LiteralExpr x) = return . show . litToBool $ x

exprEvalToBool :: Expr -> Reader' Bool
exprEvalToBool (FieldNum n) = undefined
exprEvalToBool (And x y) = do
      x' <- exprEvalToBool x 
      if x' then exprEvalToBool y else return False
exprEvalToBool (Or x y) = do
      x' <- exprEvalToBool x
      if x' then return True else exprEvalToBool y
exprEvalToBool (Compare op x y) = undefined
exprEvalToBool x@(StringChoice _) = (litToBool . LitString) <$> (exprEvalToString x)
exprEvalToBool (LiteralExpr x) = return $ litToBool x

litToBool :: Literal -> Bool
litToBool (LitString "") = False
litToBool (LitBool False) = False
litToBool LitNull = False
litToBool _ = True


