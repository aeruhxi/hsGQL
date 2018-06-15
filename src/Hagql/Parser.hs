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
import           Data.Functor                   ( void )
import           Data.Maybe                     ( maybeToList )


-- Root Document Parser
document :: Parser Document
document = many definition

definition :: Parser Definition
definition =
  (ExecutableDefinition <$> executableDefinition)
    <|> (TypeSystemDefinition <$> typeSystemDefinition)

executableDefinition :: Parser ExecutableDefinition
executableDefinition = operationDefinition <|> fragmentDefinition

typeSystemDefinition :: Parser TypeSystemDefinition
typeSystemDefinition =
  try schemaDefinition
    <|> try scalarTypeDefinition
    <|> try objectTypeDefinition
    <|> try unionTypeDefinition
    <|> try enumTypeDefinition
    <|> try inputObjectTypeDefinition

-- SchemaDefinition Parser
------------------------------------------------------------------------------

schemaDefinition :: Parser TypeSystemDefinition
schemaDefinition = do
  symbol "schema"
  dirs  <- optional directives
  rotds <- braces $ some rootOperationTypeDefinition
  return $ SchemaDefinition (maybeToList dirs) rotds

rootOperationTypeDefinition :: Parser RootOperationTypeDefinition
rootOperationTypeDefinition = do
  op <- operationType
  symbol ":"
  t <- name
  let op' = case op of
        "query"    -> Query
        "mutation" -> Mutation
        _          -> Subscription
  return $ RootOperationTypeDefinition op' t


-- Type Definition Parser
--------------------------------------------------------------------------------

scalarTypeDefinition :: Parser TypeSystemDefinition
scalarTypeDefinition = do
  desc <- optional description
  symbol "scalar"
  name <- name
  dirs <- optional directives
  return $ ScalarTypeDefinition desc name (maybeToList dirs)

objectTypeDefinition :: Parser TypeSystemDefinition
objectTypeDefinition = do
  desc <- optional description
  symbol "type"
  name <- name
  imp  <- optional implementsInterfaces
  dirs <- optional directives
  fds  <- optional fieldDefinitions
  return $ ObjectTypeDefinition desc
                                name
                                (maybeToList imp)
                                (maybeToList dirs)
                                (maybeToList fds)

unionTypeDefinition :: Parser TypeSystemDefinition
unionTypeDefinition = do
  desc <- optional description
  symbol "union"
  n    <- name
  dirs <- optional directives
  un   <- optional unionMemberTypes
  return $ UnionTypeDefinition desc n (maybeToList dirs) (maybeToList un)

enumTypeDefinition :: Parser TypeSystemDefinition
enumTypeDefinition = do
  desc <- optional description
  symbol "enum"
  n    <- name
  dirs <- optional directives
  evs  <- optional $ braces (many enumValueDefinition)
  return $ EnumTypeDefinition desc n (maybeToList dirs) (maybeToList evs)

inputObjectTypeDefinition :: Parser TypeSystemDefinition
inputObjectTypeDefinition = do
  desc <- optional description
  symbol "input"
  n    <- name
  dirs <- optional directives
  ifds <- optional $ braces (many inputValueDefinition)
  return
    $ InputObjectTypeDefinition desc n (maybeToList dirs) (maybeToList ifds)

-- Utilities

implementsInterfaces :: Parser ImplementsInterfaces
implementsInterfaces = do
  symbol "implements"
  optional $ symbol "&"
  first <- name
  rest  <- many $ symbol "&" >> name
  return $ first : rest

fieldDefinition :: Parser FieldDefinition
fieldDefinition = do
  desc  <- optional description
  name  <- name
  argsd <- optional argumentsDefinition
  symbol ":"
  t    <- variableType
  dirs <- optional directives
  return $ FieldDefinition desc name (maybeToList argsd) t (maybeToList dirs)

fieldDefinitions :: Parser [FieldDefinition]
fieldDefinitions = braces (many fieldDefinition)

inputValueDefinition :: Parser InputValueDefinition
inputValueDefinition = do
  desc <- optional description
  name <- name
  symbol ":"
  t    <- variableType
  df   <- optional defaultValue
  dirs <- optional directives
  return $ InputValueDefinition desc name t df (maybeToList dirs)

argumentsDefinition :: Parser ArgumentsDefinition
argumentsDefinition = parens (many inputValueDefinition)

description :: Parser Text
description = stringLiteral

unionMemberTypes :: Parser UnionMemberTypes
unionMemberTypes = do
  symbol "="
  optional $ symbol "|"
  first <- name
  rest  <- many $ symbol "|" >> name
  return $ first : rest

enumValueDefinition :: Parser EnumValueDefinition
enumValueDefinition = do
  desc <- optional description
  ev   <- enumValue
  dirs <- optional directives
  return $ EnumValueDefinition desc ev (maybeToList dirs)

enumValue :: Parser EnumValue
enumValue = lexeme . try $ do
  x <- name
  if x `elem` ["true", "false", "null"]
    then fail $ "enum " ++ show x ++ " cannot be one of true, false or null"
    else return x

-- Executable Definition Parser
--------------------------------------------------------------------------------

operationDefinition :: Parser ExecutableDefinition
operationDefinition = do
  qm   <- operationType
  n    <- optional name
  vd   <- optional variableDefinitions
  dirs <- optional directives
  sels <- selectionSet

  let operationType = case qm of
        "query"    -> Query
        "mutation" -> Mutation
        _          -> Subscription

  return $ Operation operationType n (maybeToList vd) (maybeToList dirs) sels

field :: Parser Selection
field = do
  x   <- name
  col <- optional colon
  case col of
    Nothing -> do
      args <- optional arguments
      dirs <- optional directives
      sels <- optional selectionSet
      return $ Field Nothing
                     x
                     (maybeToList args)
                     (maybeToList dirs)
                     (maybeToList sels)

    Just _ -> do
      y    <- name
      args <- optional arguments
      dirs <- optional directives
      sels <- optional selectionSet
      return $ Field (Just x)
                     y
                     (maybeToList args)
                     (maybeToList dirs)
                     (maybeToList sels)

fragmentDefinition :: Parser ExecutableDefinition
fragmentDefinition = do
  symbol "fragment"
  name <- fragmentName
  symbol "on"
  typeCond <- namedType
  dirs     <- optional directives
  sels     <- selectionSet
  return $ Fragment name typeCond (maybeToList dirs) sels

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
      n    <- namedType
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
  (Variable <$> variable)
    <|> (FloatValue <$> float)
    <|> (IntValue <$> signedInteger)
    <|> (BooleanValue <$> bool)
    <|> (StringValue <$> stringLiteral)
    <|> (NullValue <$ null)
    <|> (ObjectValue <$> objectFields)
    <|> (ListValue <$> listValues)
    <|> (EnumValue <$> enumValue)

objectFields :: Parser [ObjectField]
objectFields =
  try (symbol "{" >> symbol "}" >> return []) <|> braces (many objectField)

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
  df <- optional defaultValue
  return $ VariableDefinition v t df

defaultValue :: Parser Value
defaultValue = symbol "=" >> value

variableType :: Parser VariableType
variableType = try nonNullType <|> namedType <|> listType

nonNullType :: Parser VariableType
nonNullType = do
  v <- namedType <|> listType
  char '!'
  return $ NonNullType v

listType :: Parser VariableType
listType = ListType <$> brackets variableType

namedType :: Parser VariableType
namedType = NamedType <$> name

operationType :: Parser Text
operationType = symbol "query" <|> symbol "mutation" <|> symbol "subscription"
