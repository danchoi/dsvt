{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Options.Applicative ((<>))
import qualified Options.Applicative as O
import Control.Applicative
import Expression
import Eval
import Template

data Options = Options {
      splitOnDelimiter :: Text -> [Text]
    , prettyIndent :: Bool
    , compact :: Bool  
    , templateOpt :: TemplateOpt
    }

data TemplateOpt = TemplateFile FilePath | TemplateString String

options :: O.Parser Options
options = Options 
    <$> ((mkDelimiter <$> 
          (O.strOption 
            (O.short 'F' 
            <> O.metavar "DELIMITER" 
            <> O.help "Default: whitespace")))
        <|> pure T.words)
    <*> O.flag False True 
        (O.short 'i' <> O.help "Pretty-indent HTML output. Default: False")
    <*> O.flag False True 
        (O.short 'c' <> O.help "Compact HTML - no line breaks between items")
    <*> ( (TemplateFile <$> 
          (O.strArgument 
            ( O.metavar "TEMPLATE-FILE" 
            <> O.help "Template file path")))
        <|> 
          (TemplateString <$>
            (O.strOption 
              (O.short 'e'
              <> O.metavar "TEMPLATE-STRING"
              <> O.help "Template as inline string"
              )
            ))
        )

opts :: O.ParserInfo Options
opts = O.info (O.helper <*> options) 
          (O.fullDesc <> O.header "dsvt"
          <> O.progDesc "DSV templating")

mkDelimiter :: String -> (Text -> [Text])
mkDelimiter s = T.splitOn (T.pack s)

main = do
    Options{..} <- O.execParser opts
    template <- case templateOpt of
                  TemplateFile f -> readFile f
                  TemplateString s -> return s
    xs <- TL.lines <$> TL.getContents
    res <- 
      mapM (\line -> 
            let fs' = splitOnDelimiter $ TL.toStrict line
                c = defContext { fields = fs' }
            in templateLine prettyIndent template c
          ) xs
    mapM_ (if compact then putStr else putStrLn) res



