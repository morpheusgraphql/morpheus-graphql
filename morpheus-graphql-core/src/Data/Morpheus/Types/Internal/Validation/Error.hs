{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Validation.Error
  ( MissingRequired (..),
    KindViolation (..),
    Unknown (..),
    Unused (..),
  )
where

import Data.Morpheus.Error.Selection (unknownSelectionField)
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    ArgumentDefinition,
    Arguments,
    Directive (..),
    DirectiveDefinition (..),
    FieldDefinition (..),
    FieldName,
    Fragment (..),
    IMPLEMENTABLE,
    IN,
    OUT,
    Object,
    ObjectEntry (..),
    Ref (..),
    TypeCategory,
    TypeDefinition,
    TypeName,
    TypeRef (..),
    ValidationError,
    Variable (..),
    VariableDefinitions,
    at,
    atPositions,
    getOperationName,
    msgValidation,
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( CurrentSelection (..),
    InputContext (..),
    OperationContext (..),
    Scope (..),
    ScopeKind (..),
    renderInputPrefix,
  )
import Relude

class Unused ctx c where
  unused :: ctx -> c -> ValidationError

-- query M ( $v : String ) { a } -> "Variable \"$bla\" is never used in operation \"MyMutation\".",
instance Unused (OperationContext s1 s2) (Variable s) where
  unused
    OperationContext {selection = CurrentSelection {operationName}}
    Variable {variableName, variablePosition} =
      ( "Variable " <> msgValidation ("$" <> variableName)
          <> " is never used in operation "
          <> msgValidation (getOperationName operationName)
          <> "."
      )
        `at` variablePosition

instance Unused (OperationContext s1 s2) (Fragment s) where
  unused
    _
    Fragment {fragmentName, fragmentPosition} =
      ( "Fragment " <> msgValidation fragmentName
          <> " is never used."
      )
        `at` fragmentPosition

class MissingRequired c ctx where
  missingRequired :: Scope -> ctx -> Ref FieldName -> c -> ValidationError

instance MissingRequired (Arguments s) ctx where
  missingRequired
    Scope {position, kind, fieldname}
    _
    Ref {refName}
    _ =
      ( inScope kind
          <> " argument "
          <> msgValidation refName
          <> " is required but not provided."
      )
        `atPositions` position
      where
        inScope DIRECTIVE = "Directive " <> msgValidation ("@" <> fieldname)
        inScope _ = "Field " <> msgValidation fieldname

instance MissingRequired (Object s) (InputContext ctx) where
  missingRequired
    Scope {position}
    ctx
    Ref {refName}
    _ =
      ( renderInputPrefix
          ctx
          <> "Undefined Field "
          <> msgValidation refName
          <> "."
      )
        `atPositions` position

instance MissingRequired (VariableDefinitions s) (OperationContext s1 s2) where
  missingRequired
    _
    OperationContext
      { selection = CurrentSelection {operationName}
      }
    Ref {refName, refPosition}
    _ =
      ( "Variable "
          <> msgValidation refName
          <> " is not defined by operation "
          <> msgValidation (getOperationName operationName)
          <> "."
      )
        `at` refPosition

class Unknown (a :: *) ref ctx where
  -- type UnknownSelector c
  unknown :: Scope -> ctx -> f a -> ref -> ValidationError

-- {...H} -> "Unknown fragment \"H\"."
instance Unknown (Fragment s) (Ref FieldName) ctx where
  unknown _ _ _ Ref {refName, refPosition} =
    ("Unknown Fragment " <> msgValidation refName <> ".") `at` refPosition

instance Unknown (TypeDefinition k s) (Ref TypeName) ctx where
  unknown _ _ _ Ref {refName, refPosition} =
    ("Unknown type " <> msgValidation refName <> ".") `at` refPosition

instance Unknown (ArgumentDefinition s) (Argument s') ctx where
  unknown Scope {kind, fieldname} _ _ Argument {argumentName, argumentPosition} =
    ( "Unknown Argument "
        <> msgValidation argumentName
        <> " on "
        <> scope kind
        <> " "
        <> msgValidation fieldname
        <> "."
    )
      `at` argumentPosition
    where
      scope DIRECTIVE = "Directive"
      scope _ = "Field"

instance Unknown (FieldDefinition IN s) (ObjectEntry valueS) (InputContext ctx) where
  unknown
    Scope {position}
    ctx
    _
    ObjectEntry {entryName} =
      ( renderInputPrefix ctx
          <> "Unknown Field "
          <> msgValidation entryName
          <> "."
      )
        `atPositions` position

instance Unknown (DirectiveDefinition s) (Directive s') ctx where
  unknown _ _ _ Directive {directiveName, directivePosition} =
    ("Unknown Directive " <> msgValidation directiveName <> ".") `at` directivePosition

instance Unknown (FieldDefinition OUT s) (Ref FieldName) ctx where
  unknown Scope {currentTypeName} _ _ = unknownSelectionField currentTypeName

class KindViolation (t :: TypeCategory) ctx where
  kindViolation :: c t -> ctx -> ValidationError

instance KindViolation IMPLEMENTABLE (Fragment s) where
  kindViolation _ Fragment {fragmentName, fragmentType, fragmentPosition} =
    ( "Fragment "
        <> msgValidation fragmentName
        <> " cannot condition on non composite type "
        <> msgValidation fragmentType
        <> "."
    )
      `at` fragmentPosition

instance KindViolation IN (Variable s) where
  kindViolation
    _
    Variable
      { variableName,
        variablePosition,
        variableType = TypeRef {typeConName}
      } =
      ( "Variable "
          <> msgValidation ("$" <> variableName)
          <> " cannot be non-input type "
          <> msgValidation typeConName
          <> "."
      )
        `at` variablePosition
