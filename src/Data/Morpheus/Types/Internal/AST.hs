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
  , getInputTypeName

  -- Selection
  , Argument(..)
  , Arguments
  , SelectionSet
  , SelectionRec(..)
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
  , DataScalar
  , DataEnum
  , DataObject
  , DataArgument
  , DataUnion
  , DataArguments
  , DataField(..)
  , DataTypeContent(..)
  , DataType(..)
  , DataTypeLib(..)
  , DataTypeWrapper(..)
  , DataValidator(..)
  , DataTypeKind(..)
  , DataFingerprint(..)
  , RawDataType(..)
  , ResolverKind(..)
  , TypeWrapper(..)
  , TypeAlias(..)
  , ArgsType(..)
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
  , getDataType
  , lookupDataObject
  , lookupDataUnion
  , lookupType
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
  -- LOCAL
  , GQLQuery(..)
  , Variables
  )
where

import           Data.Map                       ( Map )
import           Language.Haskell.TH.Syntax     ( Lift )

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

getInputTypeName :: Object stage -> Either Message (Name, Value stage)
getInputTypeName = inputtype
 where
  inputtype [("__typename", tName), (name, value)] =
    inputtypeName tName name value
  inputtype [(name, value), ("__typename", tName)] =
    inputtypeName tName name value
  inputtype [_, _] = failure ("__typename not found on Input Union" :: Message)
  inputtype _      = failure ("only one field can be provided" :: Message)
  --------------------------
  inputtypeName (Enum name) fieldName fieldValue
    | fieldName == name = pure (name, fieldValue)
    | otherwise = failure ("field " <> name <> "was not providedm" :: Message)
  inputtypeName _ _ _ = failure ("__typename must be Enum" :: Message)
