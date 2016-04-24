{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Options.Applicative ((<>))
import qualified Options.Applicative as O
import Control.Applicative
import Expression
import Eval
import Control.Monad.Reader

data Options = Options {
      exprString :: String
    , mode :: Mode
    , fields' :: [Text]
    }

data Mode = EvalString | EvalBool

options :: O.Parser Options
options = Options 
    <$> O.strArgument 
        ( O.metavar "EXPRESSION"
        <> O.help "Expression")
    <*> O.flag EvalString EvalBool 
        ( O.short 'b'
        <> O.help "Evaluate to Bool; default String")
    <*> (((T.words . T.pack)  <$> 
              O.strArgument 
              ( O.metavar "FIELDS" 
              <> O.help "Test field values, space-delimited")
         ) <|> pure [])

opts :: O.ParserInfo Options
opts = O.info (O.helper <*> options) 
          (O.fullDesc <> O.header "dsvt-test"
          <> O.progDesc "DSV templating")

main = do
    Options{..} <- O.execParser opts
    let expr' :: Expr
        expr' = runParse expr exprString
    let context = defContext { fields = fields' }
    print expr'
    case mode of
      EvalString -> do
          let r = runReader (exprEvalToString expr') context
          print r
      EvalBool -> do
          let r = runReader (exprEvalToBool expr') context
          print r



