{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import qualified Data.Map as M
import Options.Applicative ((<>))
import qualified Options.Applicative as O
import Control.Applicative
import Text.Parsec hiding (many, (<|>))
import Data.Functor.Identity (Identity)

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

runParse :: ExprParser a -> String -> a
runParse parser inp =
  case Text.Parsec.parse parser "" inp of
    Left x -> error $ "parser failed: " ++ show x
    Right xs -> xs


------------------------------------------------------------------------     
-- expression parsers

-- Fields are accessed with $NUM where NUM is an integer, starting at 1,
-- like awk.

data Expr = FieldNum Int  -- access to DSV fields
          | And Expr Expr
          | Or Expr Expr
          | Compare String Expr Expr
          | StringChoice (M.Map String Expr)  -- usually for css class selection
          | LiteralExpr Literal -- used in the context of comparison
          deriving (Eq, Show)

data Literal = LitString String | LitNumber Int | LitBool Bool | LitNull
    deriving (Eq, Show)

data TextChunk = PassThrough String | Interpolation String 
    deriving (Show, Eq)

data ComparableValue = ComparableNumberValue Int
                     | ComparableStringValue String
                     | ComparableBoolValue Bool
                     | ComparableNullValue 
    deriving (Ord, Eq, Show)

type ExprParser = ParsecT String () Identity 

expr = do
    stringChoice <|> (do
      expr1 <- exprTerm
      try (do symbol "&&"; expr2 <- expr; return $ And expr1 expr2) 
       <|> try (do symbol "||"; expr2 <- expr; return $ Or expr1 expr2) 
       <|> try (do op <- comparisonOp; expr2 <- expr; return $ Compare op expr1 expr2) 
       <|> return expr1)

stringChoice = do
    char '{' >> spaces
    pairs :: [(String, Expr)] <- 
          sepBy1 ((,) <$> varName <*> (char ':' >> spaces >> expr <* spaces)) 
                 (char ',' >> spaces)
    spaces >> char '}' >> spaces
    return $ StringChoice $ M.fromList pairs

symbol :: String -> ExprParser String
symbol s = spaces *> string s <* spaces

comparisonOp = choice $ map (try . symbol) [">=", "<=", "!=", ">", "<", "=="]

exprTerm :: ExprParser Expr
exprTerm = (char '(' *> expr <* char ')') <|> literalExpr <|> fieldNum

fieldNum :: ExprParser Expr
fieldNum = (FieldNum . read) <$> (char '$' *> many1 digit)

varName = many1 (alphaNum <|> char '$' <|> char '_')

literalExpr = LiteralExpr <$> (litNumber <|> litString <|> litBool <|> litNull)

litNull = pure LitNull <* string "null"

litNumber = LitNumber . read <$> many1 digit  

-- dumb simple implementation that does not deal with escaping
litString = LitString <$> ((char '"' *> many (noneOf "\"") <* char '"') <|> (char '\'' *> many (noneOf "'") <* char '\''))

litBool = LitBool <$> ((try (string "true") *> pure True) <|> (try (string "false") *> pure False))

------------------------------------------------------------------------
-- free text interpolation

parseText :: String -> [TextChunk]
parseText = runParse (many textChunk) 

textChunk :: ExprParser TextChunk
textChunk = interpolationChunk <|> passThroughChunk

interpolationChunk = do
    try (string "{{")
    spaces
    xs <- manyTill anyChar (lookAhead $ try (string "}}"))
    spaces
    string "}}"
    return $ Interpolation xs

passThroughChunk = PassThrough <$> passThrough

passThrough = do
    -- a lead single { char. This is guaranteed not to be part of {{
    t <- optionMaybe (string "{") 
    xs <- many1 (noneOf "{") 
    x <- (eof *> pure "{{") <|> lookAhead (try (string "{{") <|> string "{")
    res <- case x of 
              "{{" -> return []
              "{" -> ('{':) <$> passThrough 
    return $ (fromMaybe "" t) ++ xs ++ res

------------------------------------------------------------------------



