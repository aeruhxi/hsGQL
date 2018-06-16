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
import           Hagql.Util                     ( listFromMaybe )

-- Root Document Parser
document :: Parser Document
document = some definition

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
  dirs  <- directives
  rotds <- rootOperationTypeDefinitions
  return $ SchemaDefinition dirs rotds

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

rootOperationTypeDefinitions :: Parser [RootOperationTypeDefinition]
rootOperationTypeDefinitions = braces $ some rootOperationTypeDefinition


-- Type Definition Parser
--------------------------------------------------------------------------------

scalarTypeDefinition :: Parser TypeSystemDefinition
scalarTypeDefinition = do
  desc <- optional description
  symbol "scalar"
  name <- name
  dirs <- directives
  return $ ScalarTypeDefinition desc name dirs

objectTypeDefinition :: Parser TypeSystemDefinition
objectTypeDefinition = do
  desc <- optional description
  symbol "type"
  name <- name
  imp  <- implementsInterfaces
  dirs <- directives
  fsd  <- fieldsDefinition
  return $ ObjectTypeDefinition desc name imp dirs fsd

unionTypeDefinition :: Parser TypeSystemDefinition
unionTypeDefinition = do
  desc <- optional description
  symbol "union"
  n    <- name
  dirs <- directives
  un   <- unionMemberTypes
  return $ UnionTypeDefinition desc n dirs un

enumTypeDefinition :: Parser TypeSystemDefinition
enumTypeDefinition = do
  desc <- optional description
  symbol "enum"
  n    <- name
  dirs <- directives
  evs  <- enumValueDefinitions
  return $ EnumTypeDefinition desc n dirs evs

inputObjectTypeDefinition :: Parser TypeSystemDefinition
inputObjectTypeDefinition = do
  desc <- optional description
  symbol "input"
  n    <- name
  dirs <- directives
  ifds <- inputValueDefinitions
  return $ InputObjectTypeDefinition desc n dirs ifds

-- Utilities

implementsInterfaces :: Parser ImplementsInterfaces
implementsInterfaces =  fmap listFromMaybe $ optional $ do
  symbol "implements"
  optional $ symbol "&"
  first <- name
  rest  <- many $ symbol "&" >> name
  return $ first : rest

fieldDefinition :: Parser FieldDefinition
fieldDefinition = do
  desc  <- optional description
  name  <- name
  argsd <- argumentsDefinition
  symbol ":"
  t    <- variableType
  dirs <- directives
  return $ FieldDefinition desc name argsd t dirs

fieldsDefinition :: Parser [FieldDefinition]
fieldsDefinition =
  fmap listFromMaybe (optional $ braces $ many fieldDefinition)

inputValueDefinition :: Parser InputValueDefinition
inputValueDefinition = do
  desc <- optional description
  name <- name
  symbol ":"
  t    <- variableType
  df   <- optional defaultValue
  dirs <- directives
  return $ InputValueDefinition desc name t df dirs

inputValueDefinitions :: Parser [InputValueDefinition]
inputValueDefinitions =
  fmap listFromMaybe (optional $ braces $ many inputValueDefinition)

argumentsDefinition :: Parser ArgumentsDefinition
argumentsDefinition =
  fmap listFromMaybe (optional $ parens $ many inputValueDefinition)

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
  dirs <- directives
  return $ EnumValueDefinition desc ev dirs

enumValueDefinitions :: Parser [EnumValueDefinition]
enumValueDefinitions =
  fmap listFromMaybe (optional $ braces (many enumValueDefinition))

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
  vd   <- variableDefinitions
  dirs <- directives
  sels <- selectionSet

  let ot = case qm of
        "query"    -> Query
        "mutation" -> Mutation
        _          -> Subscription

  return $ Operation ot n vd dirs sels

field :: Parser Selection
field = do
  x   <- name
  col <- optional colon
  case col of
    Nothing -> do
      args <- arguments
      dirs <- directives
      sels <- selectionSet
      return $ Field Nothing x args dirs sels

    Just _ -> do
      y    <- name
      args <- arguments
      dirs <- directives
      sels <- selectionSet
      return $ Field (Just x) y args dirs sels

fragmentDefinition :: Parser ExecutableDefinition
fragmentDefinition = do
  symbol "fragment"
  name <- fragmentName
  symbol "on"
  typeCond <- namedType
  dirs     <- directives
  sels     <- selectionSet
  return $ Fragment name typeCond dirs sels

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
      dirs <- directives
      sels <- selectionSet
      return $ InlineFragment Nothing dirs sels
    Just _ -> do
      n    <- namedType
      dirs <- directives
      sels <- selectionSet
      return $ InlineFragment (Just n) dirs sels

selectionSet :: Parser SelectionSet
selectionSet = fmap listFromMaybe (optional $ braces $ many selection)

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
objectFields = braces (many objectField)

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
  return $ Directive n (listFromMaybe args)

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
