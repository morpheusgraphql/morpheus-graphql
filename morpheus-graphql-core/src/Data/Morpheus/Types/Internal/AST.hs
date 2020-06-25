{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Types.Internal.AST
  ( -- BASE
    Ref (..),
    Position (..),
    Message,
    anonymousRef,
    FieldName (..),
    Description,
    Stage,
    CONST,
    VALID,
    RAW,
    VALIDATION_MODE (..),
    -- VALUE
    Value (..),
    ScalarValue (..),
    Object,
    GQLValue (..),
    replaceValue,
    decodeScientific,
    convertToJSONName,
    convertToHaskellName,
    RawValue,
    ValidValue,
    RawObject,
    ValidObject,
    ResolvedObject,
    ResolvedValue,
    splitDuplicates,
    removeDuplicates,
    -- Selection
    Argument (..),
    Arguments,
    SelectionSet,
    SelectionContent (..),
    Selection (..),
    Fragments,
    Fragment (..),
    -- OPERATION
    Operation (..),
    Variable (..),
    VariableDefinitions,
    DefaultValue,
    getOperationName,
    getOperationDataType,
    -- DSL
    ScalarDefinition (..),
    DataEnum,
    FieldsDefinition,
    ArgumentDefinition,
    DataUnion,
    ArgumentsDefinition (..),
    FieldDefinition (..),
    InputFieldsDefinition,
    TypeContent (..),
    TypeDefinition (..),
    Schema (..),
    DataTypeWrapper (..),
    TypeKind (..),
    DataFingerprint (..),
    TypeWrapper (..),
    TypeRef (..),
    DataEnumValue (..),
    OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    Directive (..),
    TypeUpdater,
    ConsD (..),
    TypeCategory,
    DataInputUnion,
    VariableContent (..),
    TypeLib,
    initTypeLib,
    defineType,
    kindOf,
    toNullable,
    toListField,
    isObject,
    isInput,
    toHSWrappers,
    isNullable,
    toGQLWrapper,
    isWeaker,
    isSubscription,
    isOutputObject,
    isNotSystemTypeName,
    isEntNode,
    createEnumType,
    createScalarType,
    createType,
    createUnionType,
    createAlias,
    mkInputUnionFields,
    fieldVisibility,
    createEnumValue,
    insertType,
    lookupDeprecated,
    lookupDeprecatedReason,
    lookupWith,
    -- Temaplate Haskell
    hsTypeName,
    -- LOCAL
    GQLQuery (..),
    Variables,
    unsafeFromFields,
    OrderedMap,
    GQLError (..),
    GQLErrors,
    ObjectEntry (..),
    UnionTag (..),
    __inputname,
    updateSchema,
    internalFingerprint,
    ANY,
    IN,
    OUT,
    FromAny (..),
    ToAny (..),
    TRUE,
    FALSE,
    TypeName (..),
    Token,
    Msg (..),
    intercalateName,
    toFieldName,
    TypeNameRef (..),
    isEnum,
    Fields (..),
    fieldsToArguments,
    mkCons,
    mkConsEnum,
    Directives,
    DirectiveDefinitions,
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldContent (..),
    fieldContentArgs,
    mkInputValue,
    TypeNameTH (..),
    isOutput,
    mkObjectField,
    UnionMember (..),
    mkUnionMember,
    SchemaDefinitionRaw (..),
    RootOperationTypeDefinition (..),
  )
where

import Data.HashMap.Lazy (HashMap)
-- Morpheus

import Data.Morpheus.Types.Internal.AST.Base
import Data.Morpheus.Types.Internal.AST.DirectiveLocation (DirectiveLocation (..))
import Data.Morpheus.Types.Internal.AST.OrderedMap
import Data.Morpheus.Types.Internal.AST.Selection
import Data.Morpheus.Types.Internal.AST.Stage
import Data.Morpheus.Types.Internal.AST.TH
import Data.Morpheus.Types.Internal.AST.TypeSystem
import Data.Morpheus.Types.Internal.AST.Value
import Language.Haskell.TH.Syntax (Lift)

type Variables = HashMap FieldName ResolvedValue

data GQLQuery = GQLQuery
  { fragments :: Fragments,
    operation :: Operation RAW,
    inputVariables :: [(FieldName, ResolvedValue)]
  }
  deriving (Show, Lift)
