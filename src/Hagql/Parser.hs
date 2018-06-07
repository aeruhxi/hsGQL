{-# LANGUAGE OverloadedStrings #-}

module Hagql.Parser where

import           Prelude                 hiding ( null )
import           Hagql.Ast
import           Hagql.Lexer
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Control.Applicative           as A
import           Data.Int                       ( Int32 )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Hagql.Util                     ( maybeToList )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Functor                   ( void )

operationDefinition :: Parser Definition
operationDefinition = do
  qm   <- symbol "query" <|> symbol "mutation"
  n    <- optional name
  vd   <- optional variableDefinitions
  dirs <- optional directives
  sels <- selectionSet

  if (qm == "query")
    then return $ Query n (maybeToList vd) (maybeToList dirs) sels
    else return $ Mutation n (maybeToList vd) (maybeToList dirs) sels

field :: Parser Selection
field = do
  x   <- name
  col <- optional colon
  case col of
    Nothing -> do
      args <- optional arguments
      dirs <- optional directives
      sels <- optional $ selectionSet
      return $ Field Nothing
                     x
                     (maybeToList args)
                     (maybeToList dirs)
                     (maybeToList sels)

    Just _ -> do
      y    <- name
      args <- optional arguments
      dirs <- optional directives
      sels <- optional $ selectionSet
      return $ Field (Just x)
                     y
                     (maybeToList args)
                     (maybeToList dirs)
                     (maybeToList sels)

fragmentSpread :: Parser Selection
fragmentSpread = symbol "..." >> FragmentSpread <$> name <*> directives

selection :: Parser Selection
selection = try inlineFragment <|> fragmentSpread <|> field

inlineFragment :: Parser Selection
inlineFragment = do
  symbol "..."
  on <- optional $ symbol "on"
  case on of
    Nothing -> do
      dirs <- optional directives
      sels <- selectionSet
      return $ InlineFragment Nothing (maybeToList dirs) sels
    Just _ -> do
      n    <- name
      dirs <- optional directives
      sels <- selectionSet
      return $ InlineFragment (Just n) (maybeToList dirs) sels

selectionSet :: Parser SelectionSet
selectionSet = braces $ many selection


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

fragmentName :: Parser Text
fragmentName = do
  n <- name
  if n == "on"
    then fail $ "Fragment name can not be on " ++ show n
    else return n

variable :: Parser Text
variable = char '$' >> name

variableDefinitions :: Parser [VariableDefinition]
variableDefinitions = parens (many variableDefinition)

variableDefinition :: Parser VariableDefinition
variableDefinition = do
  v <- variable
  colon
  t  <- variableType
  df <- optional value
  return $ VariableDefinition v t df

variableType :: Parser VariableType
variableType = try nonNullType <|> namedType <|> listType
 where
  namedType   = NamedType <$> name
  listType    = ListType <$> (brackets variableType)
  nonNullType = do
    v <- namedType <|> listType
    char '!'
    return $ NonNullType v
