{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Internal.AST.Base
  ( Key,
    Ref (..),
    Position (..),
    Message,
    Name,
    Named,
    Description,
    VALID,
    RAW,
    TypeWrapper (..),
    Stage (..),
    RESOLVED,
    TypeRef (..),
    VALIDATION_MODE (..),
    OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    DataTypeKind (..),
    DataFingerprint (..),
    DataTypeWrapper (..),
    anonymousRef,
    toHSWrappers,
    toGQLWrapper,
    sysTypes,
    isNullable,
    isWeaker,
    isSubscription,
    isOutputObject,
    isDefaultTypeName,
    isSchemaTypeName,
    isPrimitiveTypeName,
    isObject,
    isInput,
    isNullableWrapper,
    isOutputType,
    sysFields,
    typeFromScalar,
    hsTypeName,
    toOperationType,
    splitDuplicates,
    removeDuplicates,
    GQLError (..),
    GQLErrors,
    internalFingerprint,
    TRUE,
    FALSE,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))

type TRUE = 'True

type FALSE = 'False

type Key = Text

type Message = Text

type Name = Key

type Description = Key

data Stage = RAW | RESOLVED | VALID

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON, Lift)

-- Positions 2 Value withs same structire
-- but different Positions should be Equal
instance Eq Position where
  _ == _ = True

data GQLError = GQLError
  { message :: Message,
    locations :: [Position]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

type GQLErrors = [GQLError]

type RAW = 'RAW

type RESOLVED = 'RESOLVED

type VALID = 'VALID

data VALIDATION_MODE
  = WITHOUT_VARIABLES
  | FULL_VALIDATION
  deriving (Eq, Show)

data DataFingerprint = DataFingerprint Name [String] deriving (Show, Eq, Ord, Lift)

internalFingerprint :: Name -> [String] -> DataFingerprint
internalFingerprint name = DataFingerprint ("SYSTEM.INTERNAL." <> name)

data OperationType
  = Query
  | Subscription
  | Mutation
  deriving (Show, Eq, Lift)

type QUERY = 'Query

type MUTATION = 'Mutation

type SUBSCRIPTION = 'Subscription

type Named a = (Name, a)

-- Refference with Position information
--
-- includes position for debugging, where Ref "a" 1 === Ref "a" 3
--
data Ref = Ref
  { refName :: Key,
    refPosition :: Position
  }
  deriving (Show, Lift, Eq)

instance Ord Ref where
  compare (Ref x _) (Ref y _) = compare x y

anonymousRef :: Key -> Ref
anonymousRef refName = Ref {refName, refPosition = Position 0 0}

-- TypeRef
-------------------------------------------------------------------
data TypeRef = TypeRef
  { typeConName :: Name,
    typeArgs :: Maybe Name,
    typeWrappers :: [TypeWrapper]
  }
  deriving (Show, Eq, Lift)

isNullable :: TypeRef -> Bool
isNullable TypeRef {typeWrappers = typeWrappers} = isNullableWrapper typeWrappers

-- Kind
-----------------------------------------------------------------------------------
data DataTypeKind
  = KindScalar
  | KindObject (Maybe OperationType)
  | KindUnion
  | KindEnum
  | KindInputObject
  | KindList
  | KindNonNull
  | KindInputUnion
  | KindInterface
  deriving (Eq, Show, Lift)

isSubscription :: DataTypeKind -> Bool
isSubscription (KindObject (Just Subscription)) = True
isSubscription _ = False

isOutputType :: DataTypeKind -> Bool
isOutputType (KindObject _) = True
isOutputType KindUnion = True
isOutputType _ = False

isOutputObject :: DataTypeKind -> Bool
isOutputObject (KindObject _) = True
isOutputObject KindInterface = True
isOutputObject _ = False

isObject :: DataTypeKind -> Bool
isObject (KindObject _) = True
isObject KindInputObject = True
isObject KindInterface = True
isObject _ = False

isInput :: DataTypeKind -> Bool
isInput KindInputObject = True
isInput _ = False

-- TypeWrappers
-----------------------------------------------------------------------------------
data TypeWrapper
  = TypeList
  | TypeMaybe
  deriving (Show, Eq, Lift)

data DataTypeWrapper
  = ListType
  | NonNullType
  deriving (Show, Lift)

isNullableWrapper :: [TypeWrapper] -> Bool
isNullableWrapper (TypeMaybe : _) = True
isNullableWrapper _ = False

isWeaker :: [TypeWrapper] -> [TypeWrapper] -> Bool
isWeaker (TypeMaybe : xs1) (TypeMaybe : xs2) = isWeaker xs1 xs2
isWeaker (TypeMaybe : _) _ = True
isWeaker (_ : xs1) (_ : xs2) = isWeaker xs1 xs2
isWeaker _ _ = False

toGQLWrapper :: [TypeWrapper] -> [DataTypeWrapper]
toGQLWrapper (TypeMaybe : (TypeMaybe : tw)) = toGQLWrapper (TypeMaybe : tw)
toGQLWrapper (TypeMaybe : (TypeList : tw)) = ListType : toGQLWrapper tw
toGQLWrapper (TypeList : tw) = [NonNullType, ListType] <> toGQLWrapper tw
toGQLWrapper [TypeMaybe] = []
toGQLWrapper [] = [NonNullType]

toHSWrappers :: [DataTypeWrapper] -> [TypeWrapper]
toHSWrappers (NonNullType : (NonNullType : xs)) =
  toHSWrappers (NonNullType : xs)
toHSWrappers (NonNullType : (ListType : xs)) = TypeList : toHSWrappers xs
toHSWrappers (ListType : xs) = [TypeMaybe, TypeList] <> toHSWrappers xs
toHSWrappers [] = [TypeMaybe]
toHSWrappers [NonNullType] = []

isDefaultTypeName :: Key -> Bool
isDefaultTypeName x = isSchemaTypeName x || isPrimitiveTypeName x

isSchemaTypeName :: Key -> Bool
isSchemaTypeName = (`elem` sysTypes)

isPrimitiveTypeName :: Key -> Bool
isPrimitiveTypeName = (`elem` ["String", "Float", "Int", "Boolean", "ID"])

sysTypes :: [Key]
sysTypes =
  [ "__Schema",
    "__Type",
    "__Directive",
    "__TypeKind",
    "__Field",
    "__DirectiveLocation",
    "__InputValue",
    "__EnumValue"
  ]

sysFields :: [Key]
sysFields = ["__typename", "__schema", "__type"]

typeFromScalar :: Name -> Name
typeFromScalar "Boolean" = "Bool"
typeFromScalar "Int" = "Int"
typeFromScalar "Float" = "Float"
typeFromScalar "String" = "Text"
typeFromScalar "ID" = "ID"
typeFromScalar _ = "ScalarValue"

hsTypeName :: Key -> Key
hsTypeName "String" = "Text"
hsTypeName "Boolean" = "Bool"
hsTypeName name | name `elem` sysTypes = "S" <> name
hsTypeName name = name

toOperationType :: Name -> Maybe OperationType
toOperationType "Subscription" = Just Subscription
toOperationType "Mutation" = Just Mutation
toOperationType "Query" = Just Query
toOperationType _ = Nothing

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = fst . splitDuplicates

-- elems -> (unique elements, duplicate elems)
splitDuplicates :: Eq a => [a] -> ([a], [a])
splitDuplicates = collectElems ([], [])
  where
    collectElems :: Eq a => ([a], [a]) -> [a] -> ([a], [a])
    collectElems collected [] = collected
    collectElems (collected, errors) (x : xs)
      | x `elem` collected = collectElems (collected, errors <> [x]) xs
      | otherwise = collectElems (collected <> [x], errors) xs
