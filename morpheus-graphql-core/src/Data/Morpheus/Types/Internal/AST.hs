{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DeriveLift             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE NamedFieldPuns         #-}

module Data.Morpheus.Types.Internal.AST
  (
    -- BASE
    Key
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
  , Named
  , splitDuplicates
  , removeDuplicates
  -- Selection
  , Argument(..)
  , Arguments
  , SelectionSet
  , SelectionContent(..)
  , Selection(..)
  , Fragments
  , Fragment(..)
  , isOutputType
  -- OPERATION
  , Operation(..)
  , Variable(..)
  , VariableDefinitions
  , DefaultValue
  , getOperationName
  , getOperationDataType
  -- DSL
  , ScalarDefinition(..)
  , DataEnum
  , FieldsDefinition(..)
  , ArgumentDefinition
  , DataUnion
  , ArgumentsDefinition(..)
  , FieldDefinition(..)
  , InputFieldsDefinition(..)
  , TypeContent(..)
  , TypeDefinition(..)
  , Schema(..)
  , DataTypeWrapper(..)
  , DataTypeKind(..)
  , DataFingerprint(..)
  , TypeWrapper(..)
  , TypeRef(..)
  , DataEnumValue(..)
  , OperationType(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  , Meta(..)
  , Directive(..)
  , TypeUpdater
  , TypeD(..)
  , ConsD(..)
  , ClientQuery(..)
  , GQLTypeD(..)
  , ClientType(..)
  , DataInputUnion
  , VariableContent(..)
  , TypeLib
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
  , isEntNode
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
  , createEnumValue
  , insertType
  , lookupDeprecated
  , lookupDeprecatedReason
  , hasArguments
  , lookupWith
  , typeFromScalar
  -- Temaplate Haskell
  , toHSFieldDefinition
  , hsTypeName
  -- LOCAL
  , GQLQuery(..)
  , Variables
  , isNullableWrapper
  , unsafeFromFields
  , unsafeFromInputFields
  , OrderedMap
  , GQLError(..)
  , GQLErrors
  , ObjectEntry(..)
  , UnionTag(..)
  , isInputDataType
  , __inputname
  )
where

import           Data.Map                       ( Map )
import           Language.Haskell.TH.Syntax     ( Lift )

-- Morpheus
import           Data.Morpheus.Types.Internal.AST.Base
import           Data.Morpheus.Types.Internal.AST.Value
import           Data.Morpheus.Types.Internal.AST.Selection
import           Data.Morpheus.Types.Internal.AST.Data
import           Data.Morpheus.Types.Internal.AST.OrderedMap

type Variables = Map Key ResolvedValue

data GQLQuery = GQLQuery
  { fragments      :: Fragments
  , operation      :: Operation RAW
  , inputVariables :: [(Key, ResolvedValue)]
  } deriving (Show,Lift)