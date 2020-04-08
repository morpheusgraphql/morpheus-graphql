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
  , unpackInputUnion
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
  , getOperationObject
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
  , Prop(..)
  , Path
  , renderPath
  , ValidationContext(..)
  , InputSourceType(..)
  , InputSource(..)
  , isInputDataType
  , renderInputPrefix
  )
where

import           Data.Map                       ( Map )
import           Language.Haskell.TH.Syntax     ( Lift )
import           Data.Semigroup                 ( (<>) )

-- Morpheus
import           Data.Morpheus.Types.Internal.Operation  
                                                ( Listable(..)
                                                , selectBy
                                                )
import           Data.Morpheus.Types.Internal.AST.Data

import           Data.Morpheus.Types.Internal.AST.Selection

import           Data.Morpheus.Types.Internal.AST.Base

import           Data.Morpheus.Types.Internal.AST.OrderedMap

import           Data.Morpheus.Types.Internal.AST.Value

import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Failure(..) )

type Variables = Map Key ResolvedValue

data GQLQuery = GQLQuery
  { fragments      :: Fragments
  , operation      :: Operation RAW
  , inputVariables :: [(Key, ResolvedValue)]
  } deriving (Show,Lift)

unpackInputUnion
  :: forall stage. [(Name, Bool)]
  -> Object stage
  -> Either Message (Name, Maybe (Value stage))
unpackInputUnion tags hm = do
  (enum :: Value stage) <- entryValue <$> selectBy 
      ("valid input union should contain __typename and actual value" :: Message) 
      "__typename" 
      hm
  tyName <- isPosibeUnion tags enum
  case size hm of
    1 -> pure (tyName, Nothing)
    2 -> do
      value <- entryValue <$> selectBy 
          ("value for Union \""<> tyName <> "\" was not Provided.") 
          tyName 
          hm
      pure (tyName , Just value)
    _ -> failure ("more then 1 value for Union was not Provided." :: Message)

isPosibeUnion :: [(Name, Bool)] -> Value stage -> Either Message Name
isPosibeUnion tags (Enum name) = case lookup name tags of
  Nothing -> failure (name <> " is not posible union type" :: Message)
  _       -> pure name
isPosibeUnion _ _ = failure ("__typename must be Enum" :: Message)

data InputSourceType
  = SourceArgument (Argument RESOLVED)
  | SourceVariable (Variable RAW)
  deriving (Show)

data InputSource = InputSource {
  sourceType :: Maybe InputSourceType,
  sourcePath :: [Prop]
} deriving (Show)

renderInputPrefix :: InputSource -> Message
renderInputPrefix InputSource { sourcePath , sourceType } = 
  maybe "" renderSource sourceType <> renderPath sourcePath

renderSource :: InputSourceType -> Message
renderSource (SourceArgument Argument { argumentName }) 
  = "Argument \"" <> argumentName <>"\" got invalid value. "
renderSource (SourceVariable Variable { variableName })
  = "Variable \"$" <> variableName <>"\" got invalid value. "


data ValidationContext 
  = ValidationContext 
    { schema           :: Schema
    , fragments        :: Fragments
    , operationName    :: Maybe Name
    , scopeTypeName    :: Name
    , scopePosition    :: Position
    , input :: InputSource
    }
    deriving (Show)