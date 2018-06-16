{-# LANGUAGE OverloadedStrings #-}

module HsGQL.Lexer where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Void                      ( Void )
import qualified Control.Applicative           as A
import           Data.Int                       ( Int32 )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Char                      ( isSpace )
import           Data.Functor                   ( void )

type Parser = Parsec Void Text

ignored :: Parser ()
ignored = void $ takeWhile1P (Just "white space or comma") isIgnored
  where isIgnored x = isSpace x || x == ','

spaceConsumer :: Parser ()
spaceConsumer = L.space ignored lineCmnt A.empty
  where lineCmnt = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

quotes3 :: Parser a -> Parser a
quotes3 = between (symbol "\"\"\"") (symbol "\"\"\"")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

escapedChars = ['\\', '"', '/', 'b', 'f', 'n', 'r', 't']

-- For nonEscape and include \ before bfnrt
escapedChars' = ['\\', '"', '/', '\b', '\f', '\n', '\r', '\t']

escape :: Parser String
escape = do
  char '\\'
  x <- oneOf escapedChars
  return . return $ case x of
    '\\' -> x
    '"'  -> x
    '/'  -> x
    'b'  -> '\b'
    'f'  -> '\f'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

unicodeEscape :: Parser String
unicodeEscape = do
  x <- char '\\'
  y <- char 'u'
  z <- count 4 hexDigitChar
  return (x : y : z)

nonEscape :: Parser String
nonEscape = pure <$> noneOf escapedChars'

stringLiteral :: Parser Text
stringLiteral = fmap (pack . concat)
                     (q (many (try unicodeEscape <|> escape <|> nonEscape)))
  where q x = try (quotes3 x) <|> quotes x

signedInteger :: Parser Int32
signedInteger = L.signed spaceConsumer (lexeme L.decimal)

float :: Parser Double
float = try $ lexeme L.float

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

bool :: Parser Bool
bool = (True <$ symbol "true") <|> (False <$ symbol "false")

null :: Parser ()
null = void $ symbol "null"

reservedWords :: [Text]
reservedWords =
  [ "fragment"
  , "on"
  , "true"
  , "false"
  , "null"
  , "union"
  , "type"
  , "interface"
  , "implements"
  , "mutation"
  , "Int"
  , "String"
  , "Float"
  , "Boolean"
  , "ID"
  ]

rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

nameChar :: Parser Char
nameChar = char '_' <|> alphaNumChar

name :: Parser Text
name = lexeme $ pack <$> some nameChar

identifier :: Parser Text
identifier = do
  x <- name
  if x `elem` reservedWords
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x
