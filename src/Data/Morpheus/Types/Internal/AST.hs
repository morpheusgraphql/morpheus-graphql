{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveLift           #-}
{-# LANGUAGE OverloadedStrings    #-}

module Data.Morpheus.Types.Internal.AST
  (
    -- BASE
    Key
  , Collection
  , Ref(..)
  , Position(..)
  , Message
  , anonymousRef
  , Name
  , Description
  , Stage
  , RESOLVED
  , VALID
  , RAW
  , VALIDATION_MODE(..)

  -- VALUE
  , Value(..)
  , ScalarValue(..)
  , Object
  , GQLValue(..)
  , replaceValue
  , decodeScientific
  , convertToJSONName
  , convertToHaskellName
  , RawValue
  , ValidValue
  , RawObject
  , ValidObject
  , ResolvedObject
  , ResolvedValue
  , unpackInputUnion

  -- Selection
  , Argument(..)
  , Arguments
  , SelectionSet
  , SelectionContent(..)
  , ValidSelection
  , Selection(..)
  , RawSelection
  , FragmentLib
  , RawArguments
  , RawSelectionSet
  , Fragment(..)
  , RawArgument
  , ValidSelectionSet
  , ValidArgument
  , ValidArguments
  , RawSelectionRec
  , ValidSelectionRec
  , isOutputType
  -- OPERATION
  , Operation(..)
  , Variable(..)
  , ValidOperation
  , RawOperation
  , VariableDefinitions
  , ValidVariables
  , DefaultValue
  , getOperationName
  , getOperationDataType
  , getOperationObject
  -- DSL
  , ScalarDefinition(..)
  , DataEnum
  , FieldsDefinition(..)
  , DataArgument
  , DataUnion
  , ArgumentsDefinition(..)
  , FieldDefinition(..)
  , DataTypeContent(..)
  , DataType(..)
  , Schema(..)
  , DataTypeWrapper(..)
  , DataTypeKind(..)
  , DataFingerprint(..)
  , TypeWrapper(..)
  , TypeRef(..)
  , DataEnumValue(..)
  , isTypeDefined
  , initTypeLib
  , defineType
  , isFieldNullable
  , allDataTypes
  , lookupDataType
  , kindOf
  , toNullableField
  , toListField
  , isObject
  , isInput
  , toHSWrappers
  , isNullable
  , toGQLWrapper
  , isWeaker
  , isSubscription
  , isOutputObject
  , sysTypes
  , isDefaultTypeName
  , isSchemaTypeName
  , isPrimitiveTypeName
  , OperationType(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  , isEntNode
  , lookupInputType
  , coerceDataObject
  , lookupDataUnion
  , lookupField
  , lookupUnionTypes
  , lookupSelectionField
  , lookupFieldAsSelectionSet
  , createField
  , createArgument
  , createDataTypeLib
  , createEnumType
  , createScalarType
  , createType
  , createUnionType
  , createAlias
  , createInputUnionFields
  , fieldVisibility
  , Meta(..)
  , Directive(..)
  , createEnumValue
  , insertType
  , TypeUpdater
  , lookupDeprecated
  , lookupDeprecatedReason
  , TypeD(..)
  , ConsD(..)
  , ClientQuery(..)
  , GQLTypeD(..)
  , ClientType(..)
  , DataInputUnion
  , VariableContent(..)
  , checkForUnknownKeys
  , checkNameCollision
  , Collectible(..)
  , hasArguments
  , TypeLib
  , lookupWith
  -- LOCAL
  , GQLQuery(..)
  , Variables
  , isNullableWrapper
  )
where

import           Data.Map                       ( Map )
import           Language.Haskell.TH.Syntax     ( Lift )
import           Data.Semigroup                 ( (<>) )

-- Morpheus
import           Data.Morpheus.Types.Internal.AST.Data

import           Data.Morpheus.Types.Internal.AST.Selection

import           Data.Morpheus.Types.Internal.AST.Base

import           Data.Morpheus.Types.Internal.AST.Value

import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Failure(..) )

type Variables = Map Key ResolvedValue

data GQLQuery = GQLQuery
  { fragments      :: FragmentLib
  , operation      :: RawOperation
  , inputVariables :: [(Key, ResolvedValue)]
  } deriving (Show,Lift)

unpackInputUnion
  :: [(Name, Bool)]
  -> Object stage
  -> Either Message (Name, Maybe (Value stage))
unpackInputUnion tags [("__typename", enum)] = do
  tyName <- isPosibeUnion tags enum
  pure (tyName, Nothing)
unpackInputUnion tags [("__typename", enum), (name, value)] = do
  tyName <- isPosibeUnion tags enum
  inputtypeName tyName name value
unpackInputUnion tags [(name, value), ("__typename", enum)] = do
  tyName <- isPosibeUnion tags enum
  inputtypeName tyName name value
unpackInputUnion _ _ = failure
  ("valid input union should contain __typename and actual value" :: Message)

isPosibeUnion :: [(Name, Bool)] -> Value stage -> Either Message Name
isPosibeUnion tags (Enum name) = case lookup name tags of
  Nothing -> failure (name <> " is not posible union type" :: Message)
  _       -> pure name
isPosibeUnion _ _ = failure ("__typename must be Enum" :: Message)


inputtypeName
  :: Name -> Name -> Value stage -> Either Message (Name, Maybe (Value stage))
inputtypeName name fName fieldValue
  | fName == name = pure (name, Just fieldValue)
  | otherwise = failure ("field \"" <> name <> "\" was not provided" :: Message)
