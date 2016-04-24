{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Expression where
import Text.Parsec hiding (many, (<|>))
import Control.Applicative
import Text.Parsec hiding (many, (<|>))
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

runParse :: ExprParser a -> String -> a
runParse parser inp =
  case Text.Parsec.parse parser "" inp of
    Left x -> error $ "parser failed: " ++ show x
    Right xs -> xs


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

data Literal = LitString String | LitNumber Double
    deriving (Eq, Show)

data TextChunk = PassThrough String | Interpolation String 
    deriving (Show, Eq)

data ComparableValue = ComparableNumber Double
                     | ComparableString Text
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

comparisonOp = choice $ map (try . symbol) [">=", "<=", "!=", "/=", "<>", ">", "<", "=="]

exprTerm :: ExprParser Expr
exprTerm = (char '(' *> expr <* char ')') <|> fieldNum <|> literalExpr 

fieldNum :: ExprParser Expr
fieldNum = (FieldNum . read) <$> (char '$' *> many1 digit)

varName = many1 (alphaNum <|> char '$' <|> char '_')

literalExpr = LiteralExpr <$> (litNumber <|> litString)


-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
------------------------------------------------------------------------

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number = many1 digit

plus = char '+' *> number

minus = char '-' <:> number

integer = plus <|> minus <|> number

float = fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Double
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer
------------------------------------------------------------------------

litNumber :: ExprParser Literal
litNumber = LitNumber <$> float

-- dumb simple implementation that does not deal with escaping
litString :: ExprParser Literal
litString = LitString <$> ((char '"' *> many (noneOf "\"") <* char '"') <|> (char '\'' *> many (noneOf "'") <* char '\''))


------------------------------------------------------------------------
-- free text interpolation
{-
interpolateValues :: ArrowXml a => Value -> a XmlTree XmlTree
interpolateValues context = 
      ((changeText (interpolateText context)) `when` isText)
      >>>
      (processAttrl (changeAttrValue (interpolateText context)) `when` isElem)
 
interpolateText context = mconcat .  map (evalText context) .  parseText
-}

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


