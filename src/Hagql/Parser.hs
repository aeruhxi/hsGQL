{-# LANGUAGE OverloadedStrings #-}

module Hagql.Parser where

import           Prelude                 hiding ( null )
import           Hagql.Ast
import           Hagql.Lexer
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

-- parseSelectionSet :: Parser SelectionSet
-- parseSelectionSet = braces $ do
--   name <- identifier


-- parseArgument :: Parser Argument
-- parseArgument = parens $ do
--   name <- many alphaNumChar
--   colon
--   value <- many


parseValue :: Parser Value
parseValue =
  (FloatValue <$> float)
    <|> (IntValue <$> signedInteger)
    <|> (BooleanValue <$> bool)
    <|> (StringValue <$> stringLiteral)
    <|> (NullValue <$ null)
    <|> (ObjectValue <$> objectFields)
    <|> (ListValue <$> listValues)
    <|> (EnumValue <$> enum)

objectFields :: Parser [ObjectField]
objectFields =
  try (symbol "{" >> symbol "}" >> return [])
    <|> (braces (sepBy objectField comma))

objectField :: Parser ObjectField
objectField = do
  x <- name
  colon
  v <- parseValue
  pure $ ObjectField (pack x) v

listValues :: Parser [Value]
listValues = brackets $ sepBy parseValue comma
