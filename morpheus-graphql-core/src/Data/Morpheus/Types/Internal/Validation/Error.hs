{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.Validation.Error
  ( MissingRequired (..),
    KindViolation (..),
    Unknown (..),
    Target (..),
    Unused (..),
  )
where

-- MORPHEUS
import Data.Maybe (maybeToList)
import Data.Morpheus.Error.Selection (unknownSelectionField)
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
    IN,
    OUT,
    Object,
    ObjectEntry (..),
    Ref (..),
    Schema,
    TypeNameRef (..),
    TypeRef (..),
    ValidationError (..),
    Variable (..),
    VariableDefinitions,
    getOperationName,
    msg,
    msgValidation,
    withPosition,
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
import Prelude (($))

class Unused ctx c where
  unused :: ctx -> c -> ValidationError

-- query M ( $v : String ) { a } -> "Variable \"$bla\" is never used in operation \"MyMutation\".",
instance Unused (OperationContext s1 s2) (Variable s) where
  unused
    OperationContext {selection = CurrentSelection {operationName}}
    Variable {variableName, variablePosition} =
      ValidationError
        { validationMessage =
            "Variable " <> msg ("$" <> variableName)
              <> " is never used in operation "
              <> msg (getOperationName operationName)
              <> ".",
          validationLocations = [variablePosition]
        }

instance Unused (OperationContext s1 s2) (Fragment s) where
  unused
    _
    Fragment {fragmentName, fragmentPosition} =
      ValidationError
        { validationMessage =
            "Fragment " <> msg fragmentName
              <> " is never used.",
          validationLocations = [fragmentPosition]
        }

class MissingRequired c ctx where
  missingRequired :: Scope -> ctx -> Ref -> c -> ValidationError

instance MissingRequired (Arguments s) ctx where
  missingRequired
    Scope {position, kind, fieldname}
    _
    Ref {refName}
    _ =
      ValidationError
        { validationMessage =
            inScope kind
              <> " argument "
              <> msg refName
              <> " is required but not provided.",
          validationLocations = maybeToList position
        }
      where
        inScope DIRECTIVE = "Directive " <> msg ("@" <> fieldname)
        inScope _ = "Field " <> msg fieldname

instance MissingRequired (Object s) (InputContext ctx) where
  missingRequired
    Scope {position}
    ctx
    Ref {refName}
    _ =
      withPosition
        position
        ( renderInputPrefix
            ctx
            <> "Undefined Field "
            <> msgValidation refName
            <> "."
        )

instance MissingRequired (VariableDefinitions s) (OperationContext s1 s2) where
  missingRequired
    _
    OperationContext
      { selection = CurrentSelection {operationName}
      }
    Ref {refName, refPosition}
    _ =
      ValidationError
        { validationMessage =
            "Variable "
              <> msg refName
              <> " is not defined by operation "
              <> msg (getOperationName operationName)
              <> ".",
          validationLocations = [refPosition]
        }

class Unknown c ref ctx where
  -- type UnknownSelector c
  unknown :: Scope -> ctx -> c -> ref -> ValidationError

-- {...H} -> "Unknown fragment \"H\"."
instance Unknown (Fragments s) Ref ctx where
  unknown _ _ _ (Ref name pos) =
    ValidationError
      { validationMessage = "Unknown Fragment " <> msg name <> ".",
        validationLocations = [pos]
      }

instance Unknown (Schema s) TypeNameRef ctx where
  unknown _ _ _ TypeNameRef {typeNameRef, typeNamePosition} =
    ValidationError
      { validationMessage = "Unknown type " <> msg typeNameRef <> ".",
        validationLocations = [typeNamePosition]
      }

instance Unknown (FieldDefinition OUT s) (Argument CONST) ctx where
  unknown _ _ FieldDefinition {fieldName} Argument {argumentName, argumentPosition} =
    ValidationError
      { validationMessage = "Unknown Argument " <> msg argumentName <> " on Field " <> msg fieldName <> ".",
        validationLocations = [argumentPosition]
      }

instance Unknown (FieldsDefinition IN s) (ObjectEntry valueS) (InputContext ctx) where
  unknown
    Scope {position}
    ctx
    _
    ObjectEntry {entryName} =
      withPosition position $
        renderInputPrefix ctx <> "Unknown Field " <> msgValidation entryName <> "."

instance Unknown (DirectiveDefinition s) (Argument s') ctx where
  unknown _ _ DirectiveDefinition {directiveDefinitionName} Argument {argumentName, argumentPosition} =
    ValidationError
      { validationMessage = "Unknown Argument " <> msg argumentName <> " on Directive " <> msg directiveDefinitionName <> ".",
        validationLocations = [argumentPosition]
      }

instance Unknown (DirectiveDefinitions s) (Directive s') ctx where
  unknown _ _ _ Directive {directiveName, directivePosition} =
    ValidationError
      { validationMessage = "Unknown Directive " <> msg directiveName <> ".",
        validationLocations = [directivePosition]
      }

instance Unknown (FieldsDefinition OUT s) Ref (OperationContext s1 s2) where
  unknown Scope {currentTypeName} _ _ = unknownSelectionField currentTypeName

class KindViolation (t :: Target) ctx where
  kindViolation :: c t -> ctx -> ValidationError

instance KindViolation 'TARGET_IMPLEMENTABLE (Fragment s) where
  kindViolation _ Fragment {fragmentName, fragmentType, fragmentPosition} =
    ValidationError
      { validationMessage =
          "Fragment "
            <> msg fragmentName
            <> " cannot condition on non composite type "
            <> msg fragmentType
            <> ".",
        validationLocations = [fragmentPosition]
      }

instance KindViolation 'TARGET_INPUT (Variable s) where
  kindViolation
    _
    Variable
      { variableName,
        variablePosition,
        variableType = TypeRef {typeConName}
      } =
      ValidationError
        { validationMessage =
            "Variable "
              <> msg ("$" <> variableName)
              <> " cannot be non-input type "
              <> msg typeConName
              <> ".",
          validationLocations = [variablePosition]
        }
