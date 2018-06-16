{-# LANGUAGE OverloadedStrings #-}

module HsGQL.Parser
  ( document
  )
where

import           Prelude                 hiding ( null )
import           HsGQL.Ast
import           HsGQL.Lexer
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Text                      ( Text )
import           HsGQL.Util                     ( listFromMaybe )

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
  _     <- symbol "schema"
  dirs  <- directives
  rotds <- rootOperationTypeDefinitions
  return $ SchemaDefinition dirs rotds

rootOperationTypeDefinition :: Parser RootOperationTypeDefinition
rootOperationTypeDefinition = do
  op <- operationType
  _  <- symbol ":"
  t  <- name
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
  _    <- symbol "scalar"
  n    <- name
  dirs <- directives
  return $ ScalarTypeDefinition desc n dirs

objectTypeDefinition :: Parser TypeSystemDefinition
objectTypeDefinition = do
  desc <- optional description
  _    <- symbol "type"
  n    <- name
  imp  <- implementsInterfaces
  dirs <- directives
  fsd  <- fieldsDefinition
  return $ ObjectTypeDefinition desc n imp dirs fsd

unionTypeDefinition :: Parser TypeSystemDefinition
unionTypeDefinition = do
  desc <- optional description
  _    <- symbol "union"
  n    <- name
  dirs <- directives
  un   <- unionMemberTypes
  return $ UnionTypeDefinition desc n dirs un

enumTypeDefinition :: Parser TypeSystemDefinition
enumTypeDefinition = do
  desc <- optional description
  _    <- symbol "enum"
  n    <- name
  dirs <- directives
  evs  <- enumValueDefinitions
  return $ EnumTypeDefinition desc n dirs evs

inputObjectTypeDefinition :: Parser TypeSystemDefinition
inputObjectTypeDefinition = do
  desc <- optional description
  _    <- symbol "input"
  n    <- name
  dirs <- directives
  ifds <- inputValueDefinitions
  return $ InputObjectTypeDefinition desc n dirs ifds

-- Utilities

implementsInterfaces :: Parser ImplementsInterfaces
implementsInterfaces = fmap listFromMaybe $ optional $ do
  _     <- symbol "implements"
  _     <- optional $ symbol "&"
  first <- name
  rest  <- many $ symbol "&" >> name
  return $ first : rest

fieldDefinition :: Parser FieldDefinition
fieldDefinition = do
  desc  <- optional description
  n     <- name
  argsd <- argumentsDefinition
  _     <- symbol ":"
  t     <- variableType
  dirs  <- directives
  return $ FieldDefinition desc n argsd t dirs

fieldsDefinition :: Parser [FieldDefinition]
fieldsDefinition =
  fmap listFromMaybe (optional $ braces $ many fieldDefinition)

inputValueDefinition :: Parser InputValueDefinition
inputValueDefinition = do
  desc <- optional description
  n    <- name
  _    <- symbol ":"
  t    <- variableType
  df   <- optional defaultValue
  dirs <- directives
  return $ InputValueDefinition desc n t df dirs

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
  _     <- symbol "="
  _     <- optional $ symbol "|"
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
  _        <- symbol "fragment"
  n        <- fragmentName
  _        <- symbol "on"
  typeCond <- namedType
  dirs     <- directives
  sels     <- selectionSet
  return $ Fragment n typeCond dirs sels

fragmentSpread :: Parser Selection
fragmentSpread = symbol "..." >> FragmentSpread <$> name <*> directives

selection :: Parser Selection
selection = try inlineFragment <|> fragmentSpread <|> field

inlineFragment :: Parser Selection
inlineFragment = do
  _  <- symbol "..."
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
  _ <- colon
  v <- value
  pure $ ObjectField x v

listValues :: Parser [Value]
listValues = brackets $ many value

directive :: Parser Directive
directive = do
  _    <- symbol "@"
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
  v  <- variable
  _  <- colon
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
  _ <- char '!'
  return $ NonNullType v

listType :: Parser VariableType
listType = ListType <$> brackets variableType

namedType :: Parser VariableType
namedType = NamedType <$> name

operationType :: Parser Text
operationType = symbol "query" <|> symbol "mutation" <|> symbol "subscription"
