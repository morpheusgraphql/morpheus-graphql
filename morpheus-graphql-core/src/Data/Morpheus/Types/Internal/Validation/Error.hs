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
    InternalError (..),
    Target (..),
    Unused (..),
  )
where

-- MORPHEUS
import Data.Maybe (maybeToList)
import Data.Morpheus.Error.Selection (unknownSelectionField)
import Data.Morpheus.Error.Utils (errorMessage)
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    CONST,
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
    Ref (..),
    Schema,
    TypeNameRef (..),
    TypeRef (..),
    Variable (..),
    VariableDefinitions,
    getOperationName,
    msg,
  )
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( TypeSystemContext (..),
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( CurrentSelection (..),
    InputContext (..),
    OperationContext (..),
    Scope (..),
    ScopeKind (..),
    Target (..),
    renderInputPrefix,
  )
import Data.Semigroup ((<>))

class InternalError a where
  internalError :: a -> GQLError

instance InternalError (FieldDefinition cat s) where
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

class Unused ctx c where
  unused :: ctx -> c -> GQLError

-- query M ( $v : String ) { a } -> "Variable \"$bla\" is never used in operation \"MyMutation\".",
instance Unused (OperationContext v) (Variable s) where
  unused
    OperationContext {selection = CurrentSelection {operationName}}
    Variable {variableName, variablePosition} =
      GQLError
        { message =
            "Variable " <> msg ("$" <> variableName)
              <> " is never used in operation "
              <> msg (getOperationName operationName)
              <> ".",
          locations = [variablePosition]
        }

instance Unused (OperationContext v) Fragment where
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
  missingRequired :: Scope -> ctx -> Ref -> c -> GQLError

instance MissingRequired (Arguments s) (OperationContext v) where
  missingRequired
    Scope {position, kind, fieldname}
    _
    Ref {refName}
    _ =
      GQLError
        { message =
            inScope kind
              <> " argument "
              <> msg refName
              <> " is required but not provided.",
          locations = maybeToList position
        }
      where
        inScope DIRECTIVE = "Directive " <> msg ("@" <> fieldname)
        inScope _ = "Field " <> msg fieldname

-- TODO: Intstance for Schema directives. detailed information
instance MissingRequired (Arguments s) (TypeSystemContext ctx) where
  missingRequired
    _
    _
    Ref {refName}
    _ =
      GQLError
        { message =
            "TODO: schema"
              <> " argument "
              <> msg refName
              <> " is required but not provided.",
          locations = []
        }

instance MissingRequired (Object s) (InputContext ctx) where
  missingRequired
    Scope {position}
    ctx
    Ref {refName}
    _ =
      GQLError
        { message =
            renderInputPrefix ctx
              <> "Undefined Field "
              <> msg refName
              <> ".",
          locations = maybeToList position
        }

instance MissingRequired (VariableDefinitions s) (OperationContext v) where
  missingRequired
    _
    OperationContext
      { selection = CurrentSelection {operationName}
      }
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

class Unknown c ref ctx where
  -- type UnknownSelector c
  unknown :: Scope -> ctx -> c -> ref -> GQLErrors

-- {...H} -> "Unknown fragment \"H\"."
instance Unknown Fragments Ref ctx where
  unknown _ _ _ (Ref name pos) =
    errorMessage
      pos
      ("Unknown Fragment " <> msg name <> ".")

instance Unknown (Schema s) TypeNameRef ctx where
  unknown _ _ _ TypeNameRef {typeNameRef, typeNamePosition} =
    errorMessage typeNamePosition ("Unknown type " <> msg typeNameRef <> ".")

instance Unknown (FieldDefinition OUT s) (Argument CONST) ctx where
  unknown _ _ FieldDefinition {fieldName} Argument {argumentName, argumentPosition} =
    errorMessage
      argumentPosition
      ("Unknown Argument " <> msg argumentName <> " on Field " <> msg fieldName <> ".")

instance Unknown (FieldsDefinition IN s) (ObjectEntry CONST) (InputContext ctx) where
  unknown
    Scope {position}
    ctx
    _
    ObjectEntry {entryName} =
      [ GQLError
          { message = renderInputPrefix ctx <> "Unknown Field " <> msg entryName <> ".",
            locations = maybeToList position
          }
      ]

instance Unknown (DirectiveDefinition s) (Argument s') ctx where
  unknown _ _ DirectiveDefinition {directiveDefinitionName} Argument {argumentName, argumentPosition} =
    errorMessage
      argumentPosition
      ("Unknown Argument " <> msg argumentName <> " on Directive " <> msg directiveDefinitionName <> ".")

instance Unknown (DirectiveDefinitions s) (Directive s') ctx where
  unknown _ _ _ Directive {directiveName, directivePosition} =
    errorMessage
      directivePosition
      ("Unknown Directive " <> msg directiveName <> ".")

instance Unknown (FieldsDefinition OUT s) Ref (OperationContext v) where
  unknown Scope {typename} _ _ = unknownSelectionField typename

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
