{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.Validation.Error
  ( MissingRequired (..),
    KindViolation (..),
    Unknown (..),
    InternalError (..),
    Target (..),
    Unused (..),
  )
where

-- MORPHEUS

import Data.Morpheus.Error.Selection (unknownSelectionField)
import Data.Morpheus.Error.Utils (errorMessage)
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    FieldDefinition (..),
    FieldsDefinition,
    Fragment (..),
    Fragments,
    GQLError (..),
    GQLErrors,
    InputFieldsDefinition,
    Object,
    ObjectEntry (..),
    RESOLVED,
    Ref (..),
    Schema,
    TypeRef (..),
    Variable (..),
    VariableDefinitions,
    getOperationName,
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( Context (..),
    InputContext (..),
    Target (..),
    renderInputPrefix,
  )
import Data.Semigroup ((<>))

class InternalError a where
  internalError :: a -> GQLError

instance InternalError FieldDefinition where
  internalError
    FieldDefinition
      { fieldName,
        fieldType = TypeRef {typeConName}
      } =
      GQLError
        { message =
            "INTERNAL: Type \"" <> typeConName
              <> "\" referenced by field \""
              <> fieldName
              <> "\" can't found in Schema ",
          locations = []
        }

class Unused c where
  unused :: Context -> c -> GQLError

-- query M ( $v : String ) { a } -> "Variable \"$bla\" is never used in operation \"MyMutation\".",
instance Unused (Variable s) where
  unused
    Context {operationName}
    Variable {variableName, variablePosition} =
      GQLError
        { message =
            "Variable \"$" <> variableName
              <> "\" is never used in operation \""
              <> getOperationName operationName
              <> "\".",
          locations = [variablePosition]
        }

instance Unused Fragment where
  unused
    _
    Fragment {fragmentName, fragmentPosition} =
      GQLError
        { message =
            "Fragment \"" <> fragmentName
              <> "\" is never used.",
          locations = [fragmentPosition]
        }

class MissingRequired c ctx where
  missingRequired :: Context -> ctx -> Ref -> c -> GQLError

instance MissingRequired (Arguments s) ctx where
  missingRequired
    Context {scopePosition, scopeSelectionName}
    _
    Ref {refName}
    _ =
      GQLError
        { message =
            "Field \"" <> scopeSelectionName <> "\" argument \""
              <> refName
              <> "\" is required but not provided.",
          locations = [scopePosition]
        }

instance MissingRequired (Object s) InputContext where
  missingRequired
    Context {scopePosition}
    inputCTX
    Ref {refName}
    _ =
      GQLError
        { message =
            renderInputPrefix inputCTX <> "Undefined Field \"" <> refName <> "\".",
          locations = [scopePosition]
        }

instance MissingRequired (VariableDefinitions s) ctx where
  missingRequired
    Context {operationName}
    _
    Ref {refName, refPosition}
    _ =
      GQLError
        { message =
            "Variable \"" <> refName
              <> "\" is not defined by operation \""
              <> getOperationName operationName
              <> "\".",
          locations = [refPosition]
        }

class Unknown c ctx where
  type UnknownSelector c
  unknown :: Context -> ctx -> c -> UnknownSelector c -> GQLErrors

-- {...H} -> "Unknown fragment \"H\"."
instance Unknown Fragments ctx where
  type UnknownSelector Fragments = Ref
  unknown _ _ _ (Ref name pos) =
    errorMessage
      pos
      ("Unknown Fragment \"" <> name <> "\".")

instance Unknown Schema ctx where
  type UnknownSelector Schema = Ref
  unknown _ _ _ Ref {refName, refPosition} =
    errorMessage refPosition ("Unknown type \"" <> refName <> "\".")

instance Unknown FieldDefinition ctx where
  type UnknownSelector FieldDefinition = Argument RESOLVED
  unknown _ _ FieldDefinition {fieldName} Argument {argumentName, argumentPosition} =
    errorMessage
      argumentPosition
      ("Unknown Argument \"" <> argumentName <> "\" on Field \"" <> fieldName <> "\".")

instance Unknown InputFieldsDefinition InputContext where
  type UnknownSelector InputFieldsDefinition = ObjectEntry RESOLVED
  unknown Context {scopePosition} ctx _ ObjectEntry {entryName} =
    [ GQLError
        { message = renderInputPrefix ctx <> "Unknown Field \"" <> entryName <> "\".",
          locations = [scopePosition]
        }
    ]

instance Unknown FieldsDefinition ctx where
  type UnknownSelector FieldsDefinition = Ref
  unknown Context {scopeTypeName} _ _ =
    unknownSelectionField scopeTypeName

class KindViolation (t :: Target) ctx where
  kindViolation :: c t -> ctx -> GQLError

instance KindViolation 'TARGET_OBJECT Fragment where
  kindViolation _ Fragment {fragmentName, fragmentType, fragmentPosition} =
    GQLError
      { message =
          "Fragment \"" <> fragmentName
            <> "\" cannot condition on non composite type \""
            <> fragmentType
            <> "\".",
        locations = [fragmentPosition]
      }

instance KindViolation 'TARGET_INPUT (Variable s) where
  kindViolation
    _
    Variable
      { variableName,
        variablePosition,
        variableType = TypeRef {typeConName}
      } =
      GQLError
        { message =
            "Variable \"$" <> variableName
              <> "\" cannot be non-input type \""
              <> typeConName
              <> "\".",
          locations = [variablePosition]
        }
