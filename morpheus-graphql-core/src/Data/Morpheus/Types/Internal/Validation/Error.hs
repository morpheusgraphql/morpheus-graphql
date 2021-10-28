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
    withPath,
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( InputContext (..),
    OperationContext (..),
    Scope (..),
    ScopeKind (..),
    renderInputPrefix,
  )
import Relude

class Unused c where
  unused :: OperationContext s1 s2 -> c -> GQLError

-- query M ( $v : String ) { a } -> "Variable \"$bla\" is never used in operation \"MyMutation\".",
instance Unused (Variable s) where
  unused
    OperationContext {operationName}
    Variable {variableName, variablePosition} =
      ( "Variable " <> msg ("$" <> variableName)
          <> " is never used in operation "
          <> msg (getOperationName operationName)
          <> "."
      )
        `at` variablePosition

instance Unused (Fragment s) where
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
    Scope {position, kind, fieldName, path}
    _
    Ref {refName}
    _ =
      ( ( inScope kind
            <> " argument "
            <> msg refName
            <> " is required but not provided."
        )
          `atPositions` position
      )
        `withPath` path
      where
        inScope DIRECTIVE = "Directive " <> msg fieldName
        inScope _ = "Field " <> msg fieldName

instance MissingRequired (Object s) (InputContext ctx) where
  missingRequired
    Scope {position, path}
    ctx
    Ref {refName}
    _ =
      ( ( renderInputPrefix
            ctx
            <> "Undefined Field "
            <> msg refName
            <> "."
        )
          `atPositions` position
      )
        `withPath` path

instance MissingRequired (VariableDefinitions s) (OperationContext s1 s2) where
  missingRequired
    Scope {path}
    OperationContext
      { operationName
      }
    Ref {refName, refPosition}
    _ =
      ( ( "Variable "
            <> msg refName
            <> " is not defined by operation "
            <> msg (getOperationName operationName)
            <> "."
        )
          `at` refPosition
      )
        `withPath` path

class Unknown ref ctx where
  unknown :: Scope -> ctx -> ref -> GQLError

-- {...H} -> "Unknown fragment \"H\"."
instance Unknown (Ref FragmentName) ctx where
  unknown Scope {path} _ Ref {refName, refPosition} =
    (("Unknown Fragment " <> msg refName <> ".") `at` refPosition) `withPath` path

instance Unknown (Ref TypeName) ctx where
  unknown Scope {path} _ Ref {refName, refPosition} =
    (("Unknown type " <> msg refName <> ".") `at` refPosition) `withPath` path

instance Unknown (Argument s') ctx where
  unknown Scope {kind, path, fieldName} _ Argument {argumentName, argumentPosition} =
    ( ( "Unknown Argument "
          <> msg argumentName
          <> " on "
          <> scope kind
          <> " "
          <> msg fieldName
          <> "."
      )
        `at` argumentPosition
    )
      `withPath` path
    where
      scope DIRECTIVE = "Directive"
      scope _ = "Field"

instance Unknown (Ref FieldName) ctx where
  unknown Scope {currentTypeName, path} _ ref =
    unknownSelectionField currentTypeName ref `withPath` path

instance Unknown (ObjectEntry valueS) (InputContext ctx) where
  unknown
    Scope {position, path}
    ctx
    ObjectEntry {entryName} =
      ( ( renderInputPrefix ctx
            <> "Unknown Field "
            <> msg entryName
            <> "."
        )
          `atPositions` position
      )
        `withPath` path

instance Unknown (Directive s') ctx where
  unknown Scope {path} _ Directive {directiveName, directivePosition} =
    (("Unknown Directive " <> msg directiveName <> ".") `at` directivePosition) `withPath` path

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
