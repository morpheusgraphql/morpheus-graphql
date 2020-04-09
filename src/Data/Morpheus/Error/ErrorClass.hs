{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.Morpheus.Error.ErrorClass
  ( MissingRequired(..)
  , KindViolation(..)
  , Unknown(..)
  , InternalError(..)
  , Target(..)
  , CTX
  )
  where


import           Data.Semigroup                 ((<>))

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Error.Selection  ( unknownSelectionField )
import           Data.Morpheus.Types.Internal.AST
                                                ( RESOLVED
                                                , Ref(..)
                                                , TypeRef(..)
                                                , GQLError(..)
                                                , GQLErrors
                                                , Argument(..)
                                                , ObjectEntry(..)
                                                , Fragment(..)
                                                , Fragments
                                                , Context(..)
                                                , Variable(..)
                                                , SelectionContext(..)
                                                , InputContext(..)
                                                , VariableDefinitions
                                                , FieldDefinition(..)
                                                , FieldsDefinition
                                                , InputFieldsDefinition
                                                , Schema
                                                , Object
                                                , Arguments
                                                , getOperationName
                                                , renderInputPrefix
                                                )

data Target 
  = TARGET_OBJECT 
  | TARGET_INPUT
--  | TARGET_UNION

class InternalError a where
  internalError :: a -> GQLError

type family   CTX a :: *
type instance CTX (Object s) = InputContext
type instance CTX (Arguments s) = SelectionContext
type instance CTX (VariableDefinitions s) = SelectionContext
type instance CTX Fragments = SelectionContext
type instance CTX Schema = SelectionContext
type instance CTX FieldDefinition = SelectionContext
type instance CTX InputFieldsDefinition = InputContext
type instance CTX FieldsDefinition = SelectionContext

instance InternalError FieldDefinition where
  internalError FieldDefinition 
    { fieldName
    , fieldType = TypeRef { typeConName } 
    } = GQLError 
      { message 
        = "INTERNAL: Type \"" <> typeConName
        <> "\" referenced by field \"" <> fieldName 
        <> "\" can't found in Schema "
      , locations = []
      }

class MissingRequired c where 
  missingRequired :: Context -> CTX c -> Ref -> c -> GQLError

instance MissingRequired (Arguments s) where
  missingRequired 
    Context { scopePosition , scopeTypeName } 
    _
    Ref { refName  } _ 
    = GQLError 
      { message 
        = "Field \"" <> scopeTypeName <> "\" argument \""
        <> refName <> "\" is required but not provided."
      , locations = [scopePosition]
      }

instance MissingRequired (Object s) where
  missingRequired 
      Context { scopePosition }
      inputCTX
      Ref { refName  } 
      _  
    = GQLError 
      { message
        =  renderInputPrefix inputCTX <> "Undefined Field \"" <> refName <> "\"."
      , locations = [scopePosition]
      }

instance MissingRequired (VariableDefinitions s) where
  missingRequired 
    _
    SelectionContext { operationName } 
    Ref { refName , refPosition } _ 
    = GQLError 
      { message 
        = "Variable \"" <> refName
        <> "\" is not defined by operation \""
        <> getOperationName operationName <> "\"."
      , locations = [refPosition]
      }


class Unknown c where
  type UnknownSelector c
  unknown :: Context -> CTX c -> c -> UnknownSelector c -> GQLErrors

-- {...H} -> "Unknown fragment \"H\"."
instance Unknown Fragments where
  type UnknownSelector Fragments = Ref
  unknown _ _ _ (Ref name pos) 
    = errorMessage pos
      ("Unknown Fragment \"" <> name <> "\".")

instance Unknown Schema where
  type UnknownSelector Schema = Ref
  unknown _ _ _ Ref { refName , refPosition }
    = errorMessage refPosition ("Unknown type \"" <> refName <> "\".")

instance Unknown FieldDefinition where
  
  type UnknownSelector FieldDefinition = Argument RESOLVED
  unknown _ _ FieldDefinition { fieldName } Argument { argumentName, argumentPosition }
    = errorMessage argumentPosition 
      ("Unknown Argument \"" <> argumentName <> "\" on Field \"" <> fieldName <> "\".")

instance Unknown InputFieldsDefinition where

  type UnknownSelector InputFieldsDefinition = ObjectEntry RESOLVED
  unknown Context { scopePosition } ctx _ ObjectEntry { entryName } = 
    [
      GQLError 
        { message = renderInputPrefix ctx <>"Unknown Field \"" <> entryName <> "\"."
        , locations = [scopePosition]
        }
    ]

instance Unknown FieldsDefinition where
  type UnknownSelector FieldsDefinition = Ref
  unknown Context { scopeTypeName } _ _ 
    = unknownSelectionField scopeTypeName

class KindViolation (t :: Target) ctx where
  kindViolation :: c t -> ctx -> GQLError

instance KindViolation 'TARGET_OBJECT Fragment where
  kindViolation _ Fragment { fragmentName, fragmentType, fragmentPosition } 
    = GQLError
    { message   
      = "Fragment \"" <> fragmentName 
        <> "\" cannot condition on non composite type \"" 
        <> fragmentType <>"\"."
    , locations = [fragmentPosition]
    }

instance KindViolation 'TARGET_INPUT (Variable s) where
  kindViolation _ Variable 
      { variableName 
      , variablePosition
      , variableType = TypeRef { typeConName }
      } 
    = GQLError 
      { message 
        =  "Variable \"$" <> variableName 
        <> "\" cannot be non-input type \""
        <> typeConName <>"\"." --TODO: render with typewrappers
      , locations = [variablePosition]
      }
