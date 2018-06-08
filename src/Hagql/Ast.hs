module Hagql.Ast where

import           Data.Text                      ( Text )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Int                       ( Int32 )
import           Data.Map                       ( Map )

type FragmentName = Text
type Name = Text
type Alias = Text
type Variable = Text

type Document = [Definition]

data Definition
  -- Operation Definition
  = Query (Maybe Name) [VariableDefinition] [Directive] SelectionSet
  | Mutation (Maybe Name) [VariableDefinition] [Directive] SelectionSet
  | Selectionset
  -- Fragment Definition
  | Fragment FragmentName VariableType [Directive] SelectionSet
  deriving (Show, Eq)

type SelectionSet = [Selection]
data Selection
  = Field (Maybe Alias) Name [Argument] [Directive] SelectionSet
  | FragmentSpread FragmentName [Directive]
  | InlineFragment (Maybe VariableType) [Directive] SelectionSet
  deriving (Show, Eq)

data Directive = Directive Name [Argument] deriving (Show, Eq)

type Argument = ObjectField

data VariableType
  = NamedType Name
  | ListType VariableType
  | NonNullType VariableType
  deriving (Show, Eq)

data VariableDefinition = VariableDefinition Variable VariableType (Maybe Value)
  deriving (Show, Eq)

data ObjectField = ObjectField Name Value deriving (Show, Eq)

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
