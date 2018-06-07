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
import           Hagql.Util                     ( maybeToList )
import qualified Text.Megaparsec.Char.Lexer    as L

field :: Parser Field
field = do
  x   <- name
  col <- optional colon
  case col of
    Nothing -> do
      args <- optional arguments
      dirs <- optional directives
      sel  <- optional $ many selection
      return $ Field Nothing
                     x
                     (maybeToList args)
                     (maybeToList dirs)
                     (maybeToList sel)

    Just _ -> do
      y    <- name
      args <- optional arguments
      dirs <- optional directives
      sel  <- optional $ many selection
      return $ Field (Just x)
                     y
                     (maybeToList args)
                     (maybeToList dirs)
                     (maybeToList sel)

selection :: Parser Selection
selection = Fields <$> (braces $ many field)

argument :: Parser Argument
argument = objectField

arguments :: Parser [Argument]
arguments = parens $ sepBy argument comma

value :: Parser Value
value =
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
  v <- value
  pure $ ObjectField x v

listValues :: Parser [Value]
listValues = brackets $ many value

directive :: Parser Directive
directive = do
  symbol "@"
  n    <- name
  args <- optional arguments
  return $ Directive n (maybeToList args)

directives :: Parser [Directive]
directives = many directive
