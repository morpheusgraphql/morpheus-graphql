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
    Arguments,
    Directive (..),
    FieldName,
    Fragment (..),
    FragmentName,
    GQLError,
    IMPLEMENTABLE,
    IN,
    Object,
    ObjectEntry (..),
    Ref (..),
    TypeCategory,
    TypeName,
    TypeRef (..),
    Variable (..),
    VariableDefinitions,
    at,
    atPositions,
    getOperationName,
    msg,
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
  unused :: ctx -> c -> GQLError

-- query M ( $v : String ) { a } -> "Variable \"$bla\" is never used in operation \"MyMutation\".",
instance Unused (OperationContext s1 s2) (Variable s) where
  unused
    OperationContext {selection = CurrentSelection {operationName}}
    Variable {variableName, variablePosition} =
      ( "Variable " <> msg ("$" <> variableName)
          <> " is never used in operation "
          <> msg (getOperationName operationName)
          <> "."
      )
        `at` variablePosition

instance Unused (OperationContext s1 s2) (Fragment s) where
  unused
    _
    Fragment {fragmentName, fragmentPosition} =
      ( "Fragment " <> msg fragmentName
          <> " is never used."
      )
        `at` fragmentPosition

class MissingRequired c ctx where
  missingRequired :: Scope -> ctx -> Ref FieldName -> c -> GQLError

instance MissingRequired (Arguments s) ctx where
  missingRequired
    Scope {position, kind, fieldname}
    _
    Ref {refName}
    _ =
      ( inScope kind
          <> " argument "
          <> msg refName
          <> " is required but not provided."
      )
        `atPositions` position
      where
        inScope DIRECTIVE = "Directive " <> msg ("@" <> fieldname)
        inScope _ = "Field " <> msg fieldname

instance MissingRequired (Object s) (InputContext ctx) where
  missingRequired
    Scope {position}
    ctx
    Ref {refName}
    _ =
      ( renderInputPrefix
          ctx
          <> "Undefined Field "
          <> msg refName
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
          <> msg refName
          <> " is not defined by operation "
          <> msg (getOperationName operationName)
          <> "."
      )
        `at` refPosition

class Unknown ref ctx where
  unknown :: Scope -> ctx -> ref -> GQLError

-- {...H} -> "Unknown fragment \"H\"."
instance Unknown (Ref FragmentName) ctx where
  unknown _ _ Ref {refName, refPosition} =
    ("Unknown Fragment " <> msg refName <> ".") `at` refPosition

instance Unknown (Ref TypeName) ctx where
  unknown _ _ Ref {refName, refPosition} =
    ("Unknown type " <> msg refName <> ".") `at` refPosition

instance Unknown (Argument s') ctx where
  unknown Scope {kind, fieldname} _ Argument {argumentName, argumentPosition} =
    ( "Unknown Argument "
        <> msg argumentName
        <> " on "
        <> scope kind
        <> " "
        <> msg fieldname
        <> "."
    )
      `at` argumentPosition
    where
      scope DIRECTIVE = "Directive"
      scope _ = "Field"

instance Unknown (Ref FieldName) ctx where
  unknown Scope {currentTypeName} _ = unknownSelectionField currentTypeName

instance Unknown (ObjectEntry valueS) (InputContext ctx) where
  unknown
    Scope {position}
    ctx
    ObjectEntry {entryName} =
      ( renderInputPrefix ctx
          <> "Unknown Field "
          <> msg entryName
          <> "."
      )
        `atPositions` position

instance Unknown (Directive s') ctx where
  unknown _ _ Directive {directiveName, directivePosition} =
    ("Unknown Directive " <> msg directiveName <> ".") `at` directivePosition

class KindViolation (t :: TypeCategory) ctx where
  kindViolation :: c t -> ctx -> GQLError

instance KindViolation IMPLEMENTABLE (Fragment s) where
  kindViolation _ Fragment {fragmentName, fragmentType, fragmentPosition} =
    ( "Fragment "
        <> msg fragmentName
        <> " cannot condition on non composite type "
        <> msg fragmentType
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
          <> msg ("$" <> variableName)
          <> " cannot be non-input type "
          <> msg typeConName
          <> "."
      )
        `at` variablePosition
