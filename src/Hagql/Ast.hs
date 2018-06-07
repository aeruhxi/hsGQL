module Hagql.Ast where

import           Data.Text                      ( Text )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Int                       ( Int32 )
import           Data.Map                       ( Map )

type FragmentName = Text
type TypeCondition = Text
type Name = Text
type Alias = Text

type Document = [Definition]

data Definition
  -- Operation Definition
  = Query (NonEmpty SelectionSet)
  | Mutation (NonEmpty SelectionSet)
  | Selectionset
  -- Fragment Definition
  | Fragment FragmentName TypeCondition SelectionSet
  deriving (Show, Eq)

type SelectionSet = [Selection]
data Selection
  = Field (Maybe Alias) Name [Argument] [Directive] SelectionSet
  | FragmentSpread FragmentName [Directive]
  | InlineFragment (Maybe TypeCondition) [Directive] SelectionSet
  deriving (Show, Eq)

data Directive = Directive Name [Argument] deriving (Show, Eq)

type Argument = ObjectField

data VariableType
  = NamedType
  | ListType
  | NonNullType
  deriving (Show, Eq)

data Variable = Variable Name VariableType deriving (Show, Eq)

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
  deriving (Show, Eq)
