module Hagql.Ast where

import           Data.Text                      ( Text )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Int                       ( Int32 )
import           Data.Map                       ( Map )

-- Root Document
type Document = [Definition]

data Definition
  = ExecutableDefinition ExecutableDefinition
  | TypeSystemDefinition TypeSystemDefinition
  -- | TypeSystemExtension TypeSystemExtension -- TODO: Implement later
  deriving  (Show, Eq)

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

data VariableDefinition = VariableDefinition Variable VariableType (Maybe DefaultValue)
  deriving (Show, Eq)


-- Directives
data Directive = Directive Name [Argument]
  deriving (Show, Eq)


-- TYPE SYSTEM

data TypeSystemDefinition
  -- Schema Definition
  = SchemaDefinition [Directive] [RootOperationTypeDefinition]
  -- Type Definition
  | ScalarTypeDefinition (Maybe Description) Name [Directive]
  | ObjectTypeDefinition (Maybe Description) Name ImplementsInterfaces [Directive] [FieldDefinition]
  | InterfaceTypeDefinition (Maybe Description) Name [Directive] [FieldDefinition]
  | UnionTypeDefinition (Maybe Description) Name [Directive] UnionMemberTypes
  | EnumTypeDefinition (Maybe Description) Name [Directive] [EnumValueDefinition]
  | InputObjectTypeDefinition (Maybe Description) Name [Directive] [InputValueDefinition]
  -- Directive Definition
  | DirectiveDefinition Name ArgumentsDefinition [DirectiveLocation]
  deriving (Show, Eq)

data RootOperationTypeDefinition
  = RootOperationTypeDefinition OperationType Name
  deriving (Show, Eq)

data FieldDefinition
  = FieldDefinition (Maybe Description) Name ArgumentsDefinition VariableType [Directive]
  deriving (Show, Eq)

data InputValueDefinition
  = InputValueDefinition (Maybe Description) Name VariableType (Maybe DefaultValue) [Directive]
  deriving (Show, Eq)

data EnumValueDefinition
  = EnumValueDefinition (Maybe Description) EnumValue [Directive]
  deriving (Show, Eq)

-- DL prefix fr directive location to avoid name clash
data DirectiveLocation
  -- ExecutableDirectiveLocation
  = DLQuery
  | DLMutation
  | DLSubscription
  | DLField
  | DLFragmentDefinition
  | DLFragmentSpread
  | DLInlineFragment
  -- TypeSystemDirectiveLocation
  | DLSchema
  | DLScalar
  | DLObject
  | DLFieldDefinition
  | DLArgumentDefinition
  | DLInterface
  | DLUnion
  | DLEnum
  | DLEnumValue
  | DLInputObject
  | DLInputFieldDefinition
  deriving (Show, Eq)

-- Aliases
type Argument = ObjectField
type FragmentName = Text
type Name = Text
type Alias = Text
type Variable = Text
type Description = Text
type ArgumentsDefinition = [InputValueDefinition]
type UnionMemberTypes = [Name]
type ImplementsInterfaces = [Name]
type DefaultValue = Value
type EnumValue = Text
