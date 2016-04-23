{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Options.Applicative ((<>))
import qualified Options.Applicative as O
import Control.Applicative
import Data.Attoparsec.Text 

data Options = Options {
      delimiter :: Text
    , templateFile :: FilePath
    }

options :: O.Parser Options
options = Options 
    <$> (T.pack <$> 
          (O.strOption 
            (O.short 's' 
            <> O.metavar "DELIMITER" 
            <> O.value "\t"
            <> O.help "Default: TAB")))
    <*> O.strArgument 
        ( O.metavar "TEMPLATE-FILE" 
        <> O.help "Template file path")

opts :: O.ParserInfo Options
opts = O.info (O.helper <*> options) 
          (O.fullDesc <> O.header "dsvt"
          <> O.progDesc "DSV templating")

main = do
    Options{..} <- O.execParser opts
    undefined
     
