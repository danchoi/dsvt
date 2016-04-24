{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Eval where
import Expression
import Control.Applicative
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Functor.Identity (Identity)
import Control.Monad.Reader
import Data.List (intersperse)
import Control.Arrow (first)
import Safe (readMay, at)

type Reader' = Reader Context 

data Context = Context {
      fields :: [Text] -- the DSV field values, as text
    , trueStrings :: [Text]
    , falseStrings :: [Text]
    , nullStrings :: [Text]
    } deriving Show

runEvalToString :: Context -> String -> String
runEvalToString c s = 
    unpack $ runReader (exprEvalToString $ runParse expr s) c

runEvalToBool :: Context -> String -> Bool
runEvalToBool c s = runReader (exprEvalToBool $ runParse expr s) c

defContext :: Context 
defContext = Context 
      [] ["True", "true", "t", "T"] ["False", "false", "f", "F"]
      ["NULL", "null"]

runEvalText :: Context -> String -> String
runEvalText c s = 
    let cs = parseText s
        t :: [Text]
        t = runReader (mapM evalText cs) c
    in unpack $ mconcat t

evalText :: TextChunk -> Reader' Text
evalText (PassThrough s) = return $ pack s
evalText (Interpolation s) = exprEvalToString (runParse expr s)

exprEvalToString :: Expr -> Reader' Text
exprEvalToString (FieldNum n) = do
      xs <- fields <$> ask 
      return $ xs `at` (n - 1)
exprEvalToString x@(And _ _) = boolToText <$> exprEvalToBool x
exprEvalToString x@(Or _ _) = boolToText <$> exprEvalToBool x
exprEvalToString x@(Compare _ _ _) = boolToText <$> exprEvalToBool x
exprEvalToString (StringChoice m) = do
      let xs :: [(Text, Expr)] = map (first pack) $ M.toList m
      trueKeys <- fmap (map fst) $ filterM (\(k, expr) -> exprEvalToBool expr) xs 
      return $
        case trueKeys of
            [] -> ""
            xs -> mconcat $ intersperse " " $ xs 
exprEvalToString (LiteralExpr x) = return . litToString $ x

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
-- see p.96 of Effective Awk Programming 3rd ed.
exprEvalToBool (Compare op x y) = do
      (vx, vy) <- comparableValues (x, y)
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
litToBool (LitNumber 0) = False
litToBool _ = True

litToString :: Literal -> Text
litToString (LitString s) = pack s
litToString (LitNumber n) = pack . show $ n

comparableValues :: (Expr, Expr) -> Reader' (ComparableValue, ComparableValue)
comparableValues (LiteralExpr (LitNumber x), y) = do
      y' <- exprEvalToString y
      let y'' = maybe (error $ "Expected number, got: " ++ show y'')
                      id $ readMay (unpack y')
      return (ComparableNumber x, ComparableNumber y'')
comparableValues (x, LiteralExpr (LitNumber y)) = do
      x' <- exprEvalToString x
      let x'' = maybe (error $ "Expected number, got: " ++ show x'')
                      id $ readMay (unpack x')
      return (ComparableNumber x'', ComparableNumber y)
comparableValues (x, y) = do
      x' <- exprEvalToString x
      y' <- exprEvalToString y
      return $ (ComparableString x', ComparableString y')




