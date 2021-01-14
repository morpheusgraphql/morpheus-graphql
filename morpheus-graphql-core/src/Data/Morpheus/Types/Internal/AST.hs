{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST
  ( Ref (..),
    Position (..),
    Message,
    anonymousRef,
    FieldName (..),
    Description,
    Stage,
    CONST,
    VALID,
    RAW,
    Value (..),
    ScalarValue (..),
    Object,
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
    Argument (..),
    Arguments,
    SelectionSet,
    SelectionContent (..),
    Selection (..),
    Fragments,
    Fragment (..),
    Operation (..),
    Variable (..),
    VariableDefinitions,
    DefaultValue,
    getOperationName,
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
    TypeWrapper (..),
    TypeRef (..),
    DataEnumValue (..),
    OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    Directive (..),
    ConsD (..),
    TypeCategory (..),
    DataInputUnion,
    VariableContent (..),
    TypeLib,
    initTypeLib,
    kindOf,
    toNullable,
    toHSWrappers,
    isNullable,
    toGQLWrapper,
    isWeaker,
    isNotSystemTypeName,
    isLeaf,
    isResolverType,
    mkEnumContent,
    createScalarType,
    mkUnionContent,
    mkTypeRef,
    mkInputUnionFields,
    fieldVisibility,
    lookupDeprecated,
    lookupDeprecatedReason,
    lookupWith,
    hsTypeName,
    ExecutableDocument (..),
    Variables,
    unsafeFromFields,
    OrdMap (..),
    GQLError (..),
    GQLErrors,
    ObjectEntry (..),
    UnionTag (..),
    ANY,
    IN,
    OUT,
    OBJECT,
    IMPLEMENTABLE,
    fromAny,
    toAny,
    TRUE,
    FALSE,
    TypeName (..),
    Token,
    Msg (..),
    intercalateName,
    toFieldName,
    fieldsToArguments,
    mkConsEnum,
    Directives,
    DirectiveDefinitions,
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldContent (..),
    fieldContentArgs,
    mkInputValue,
    mkType,
    mkObjectField,
    UnionMember (..),
    mkUnionMember,
    mkNullaryMember,
    RawTypeDefinition (..),
    RootOperationTypeDefinition (..),
    UnionSelection,
    SchemaDefinition (..),
    buildSchema,
    InternalError (..),
    ValidationError (..),
    msgInternal,
    getOperationDataType,
    Typed (Typed),
    typed,
    untyped,
    msgValidation,
    withPosition,
    ValidationErrors,
    toGQLError,
    LEAF,
    INPUT_OBJECT,
    ToCategory (..),
    FromCategory (..),
    possibleTypes,
    possibleInterfaceTypes,
    mkField,
    defineSchemaWith,
    type (<=!),
    ToOBJECT,
    constraintInputUnion,
    getInputUnionValue,
    unitFieldName,
    unitTypeName,
  )
where

import Data.Morpheus.Ext.OrdMap (OrdMap (..))
import Data.Morpheus.Ext.SafeHashMap (SafeHashMap)
import Data.Morpheus.Types.Internal.AST.Base
import Data.Morpheus.Types.Internal.AST.DirectiveLocation (DirectiveLocation (..))
import Data.Morpheus.Types.Internal.AST.Fields
import Data.Morpheus.Types.Internal.AST.Selection
import Data.Morpheus.Types.Internal.AST.Stage
import Data.Morpheus.Types.Internal.AST.TH
import Data.Morpheus.Types.Internal.AST.Type
import Data.Morpheus.Types.Internal.AST.TypeCategory
import Data.Morpheus.Types.Internal.AST.TypeSystem
import Data.Morpheus.Types.Internal.AST.Union
import Data.Morpheus.Types.Internal.AST.Value
import Language.Haskell.TH.Syntax (Lift)
import Prelude (Show)

type Variables = SafeHashMap FieldName ResolvedValue

data ExecutableDocument = ExecutableDocument
  { inputVariables :: Variables,
    operation :: Operation RAW,
    fragments :: Fragments RAW
  }
  deriving (Show, Lift)
