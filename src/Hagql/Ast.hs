module Hagql.Ast where

import           Data.Text                      ( Text )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Int                       ( Int32 )
import           Data.Map                       ( Map )

-- Root Document
type Document = [ExecutableDefinition]


-- Operations
data OperationType = Query | Mutation | Subscription
  deriving (Show, Eq)

data ExecutableDefinition
  -- Operation Definition
  =  Operation OperationType (Maybe Name) [VariableDefinition] [Directive] SelectionSet
  | Selectionset
  -- Fragment Definition
  | Fragment FragmentName VariableType [Directive] SelectionSet
  deriving (Show, Eq)


-- Selection Set
type SelectionSet = [Selection]

data Selection
  = Field (Maybe Alias) Name [Argument] [Directive] SelectionSet
  | FragmentSpread FragmentName [Directive]
  | InlineFragment (Maybe VariableType) [Directive] SelectionSet
  deriving (Show, Eq)

-- LANGUAGE

-- Fields
data FieldDefinition = FieldDefinition Description Name
  deriving (Show, Eq)


-- Input Values
data Value
  = IntValue Int32
  | FloatValue Double
  | StringValue Text
  | BooleanValue Bool
  | NullValue
  | EnumValue Text
  | ObjectValue [ObjectField]
  | ListValue [Value]
  | Variable Text
  deriving (Show, Eq)

data ObjectField = ObjectField Name Value deriving (Show, Eq)


-- Variables
data VariableType
  = NamedType Name
  | ListType VariableType
  | NonNullType VariableType
  deriving (Show, Eq)

data VariableDefinition = VariableDefinition Variable VariableType (Maybe Value)
  deriving (Show, Eq)


-- Directives
data Directive = Directive Name [Argument]
  deriving (Show, Eq)


-- Aliases
type Argument = ObjectField
type FragmentName = Text
type Name = Text
type Alias = Text
type Variable = Text
type Description = Text
