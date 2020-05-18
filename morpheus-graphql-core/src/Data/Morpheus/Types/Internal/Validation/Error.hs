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
    Directive (..),
    DirectiveDefinition (..),
    DirectiveDefinitions,
    FieldDefinition (..),
    FieldsDefinition,
    Fragment (..),
    Fragments,
    GQLError (..),
    GQLErrors,
    IN,
    OUT,
    Object,
    ObjectEntry (..),
    RAW,
    RESOLVED,
    Ref (..),
    Schema,
    TypeNameRef (..),
    TypeRef (..),
    Variable (..),
    VariableDefinitions,
    getOperationName,
    msg,
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

instance InternalError (FieldDefinition cat) where
  internalError
    FieldDefinition
      { fieldName,
        fieldType = TypeRef {typeConName}
      } =
      GQLError
        { message =
            "INTERNAL: Type " <> msg typeConName
              <> " referenced by field "
              <> msg fieldName
              <> " can't found in Schema ",
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
            "Variable " <> msg ("$" <> variableName)
              <> " is never used in operation "
              <> msg (getOperationName operationName)
              <> ".",
          locations = [variablePosition]
        }

instance Unused Fragment where
  unused
    _
    Fragment {fragmentName, fragmentPosition} =
      GQLError
        { message =
            "Fragment " <> msg fragmentName
              <> " is never used.",
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
            "Field "
              <> msg scopeSelectionName
              <> " argument "
              <> msg refName
              <> " is required but not provided.",
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
            renderInputPrefix inputCTX
              <> "Undefined Field "
              <> msg refName
              <> ".",
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
            "Variable "
              <> msg refName
              <> " is not defined by operation "
              <> msg (getOperationName operationName)
              <> ".",
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
      ("Unknown Fragment " <> msg name <> ".")

instance Unknown Schema ctx where
  type UnknownSelector Schema = TypeNameRef
  unknown _ _ _ TypeNameRef {typeNameRef, typeNamePosition} =
    errorMessage typeNamePosition ("Unknown type " <> msg typeNameRef <> ".")

instance Unknown (FieldDefinition OUT) ctx where
  type UnknownSelector (FieldDefinition OUT) = Argument RESOLVED
  unknown _ _ FieldDefinition {fieldName} Argument {argumentName, argumentPosition} =
    errorMessage
      argumentPosition
      ("Unknown Argument " <> msg argumentName <> " on Field " <> msg fieldName <> ".")

instance Unknown (FieldsDefinition IN) InputContext where
  type UnknownSelector (FieldsDefinition IN) = ObjectEntry RESOLVED
  unknown Context {scopePosition} ctx _ ObjectEntry {entryName} =
    [ GQLError
        { message = renderInputPrefix ctx <> "Unknown Field " <> msg entryName <> ".",
          locations = [scopePosition]
        }
    ]

instance Unknown DirectiveDefinition ctx where
  type UnknownSelector DirectiveDefinition = Argument RESOLVED
  unknown _ _ DirectiveDefinition {directiveDefinitionName} Argument {argumentName, argumentPosition} =
    errorMessage
      argumentPosition
      ("Unknown Argument " <> msg argumentName <> " on Directive " <> msg directiveDefinitionName <> ".")

instance Unknown DirectiveDefinitions ctx where
  type UnknownSelector DirectiveDefinitions = Directive RAW
  unknown _ _ _ Directive {directiveName} =
    errorMessage
      (undefined) -- TODO: real
      ("Unknown Directive " <> msg directiveName <> ".")

instance Unknown (FieldsDefinition OUT) ctx where
  type UnknownSelector (FieldsDefinition OUT) = Ref
  unknown Context {scopeTypeName} _ _ =
    unknownSelectionField scopeTypeName

class KindViolation (t :: Target) ctx where
  kindViolation :: c t -> ctx -> GQLError

instance KindViolation 'TARGET_OBJECT Fragment where
  kindViolation _ Fragment {fragmentName, fragmentType, fragmentPosition} =
    GQLError
      { message =
          "Fragment "
            <> msg fragmentName
            <> " cannot condition on non composite type "
            <> msg fragmentType
            <> ".",
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
            "Variable "
              <> msg ("$" <> variableName)
              <> " cannot be non-input type "
              <> msg typeConName
              <> ".",
          locations = [variablePosition]
        }
