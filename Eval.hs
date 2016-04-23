{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Eval where
import Expression
import Control.Applicative
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Functor.Identity (Identity)
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

exprEvalToString :: Expr -> Reader' Text
exprEvalToString (FieldNum n) = do
      xs <- fields <$> ask 
      return $ xs !! (n - 1)
exprEvalToString x@(And _ _) = boolToText <$> exprEvalToBool x
exprEvalToString x@(Or _ _) = boolToText <$> exprEvalToBool x
exprEvalToString x@(Compare _ _ _) = boolToText <$> exprEvalToBool x
exprEvalToString (StringChoice map) = undefined
exprEvalToString (LiteralExpr x) = return . boolToText . litToBool $ x


boolToText :: Bool -> Text 
boolToText = pack . show 

exprEvalToBool :: Expr -> Reader' Bool
exprEvalToBool x@(FieldNum _) = do
      s <- exprEvalToString x
      Context{..} <- ask
      return $ 
          (not $ T.null s) && 
          (not $ s `elem` (nullStrings ++ falseStrings))
exprEvalToBool (And x y) = do
      x' <- exprEvalToBool x 
      if x' then exprEvalToBool y else return False
exprEvalToBool (Or x y) = do
      x' <- exprEvalToBool x
      if x' then return True else exprEvalToBool y
exprEvalToBool (Compare op x y) = do
      vx <- comparableValue =<< exprEvalToString x 
      vy <- comparableValue =<< exprEvalToString y 
      return $
        case op of   
            ">" -> vx > vy
            "<" -> vx < vy
            ">=" -> vx >= vy
            "<=" -> vx <= vy
            "==" -> vx == vy
            "!=" -> vx /= vy
            "/=" -> vx /= vy
            "<>" -> vx /= vy
            x -> error $ "Unsupported comparison operator: " ++ x
exprEvalToBool x@(StringChoice _) = (litToBool . LitString . unpack) <$> (exprEvalToString x)
exprEvalToBool (LiteralExpr x) = return $ litToBool x

litToBool :: Literal -> Bool
litToBool (LitString "") = False
litToBool (LitBool False) = False
litToBool LitNull = False
litToBool _ = True

comparableValue :: Text -> Reader' ComparableValue
comparableValue "" = undefined
comparableValue x = error $ "can't make comparable value for " ++ show x


