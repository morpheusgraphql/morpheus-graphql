{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.Morpheus.Types.Internal.Validation.Error
  ( MissingRequired(..)
  , KindViolation(..)
  , Unknown(..)
  , InternalError(..)
  , Target(..)
  )
  where

import           Data.Semigroup                 ((<>))

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Error.Selection  ( unknownSelectionField )
import           Data.Morpheus.Types.Internal.Validation.Validator
                                                ( Context(..)
                                                , InputContext(..)
                                                , SelectionContext(..)
                                                , renderInputPrefix
                                                , Target(..)
                                                )
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
                                                , Variable(..)
                                                , VariableDefinitions
                                                , FieldDefinition(..)
                                                , FieldsDefinition
                                                , InputFieldsDefinition
                                                , Schema
                                                , Object
                                                , Arguments
                                                , getOperationName
                                                )



class InternalError a where
  internalError :: a -> GQLError



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

class MissingRequired c ctx where 
  missingRequired :: Context -> ctx -> Ref -> c -> GQLError

instance MissingRequired (Arguments s) ctx where
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

instance MissingRequired (Object s) InputContext where
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

instance MissingRequired (VariableDefinitions s) SelectionContext where
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


class Unknown c ctx where
  type UnknownSelector c
  unknown :: Context -> ctx -> c -> UnknownSelector c -> GQLErrors

-- {...H} -> "Unknown fragment \"H\"."
instance Unknown Fragments ctx where
  type UnknownSelector Fragments = Ref
  unknown _ _ _ (Ref name pos) 
    = errorMessage pos
      ("Unknown Fragment \"" <> name <> "\".")

instance Unknown Schema ctx where
  type UnknownSelector Schema = Ref
  unknown _ _ _ Ref { refName , refPosition }
    = errorMessage refPosition ("Unknown type \"" <> refName <> "\".")

instance Unknown FieldDefinition ctx where
  type UnknownSelector FieldDefinition = Argument RESOLVED
  unknown _ _ FieldDefinition { fieldName } Argument { argumentName, argumentPosition }
    = errorMessage argumentPosition 
      ("Unknown Argument \"" <> argumentName <> "\" on Field \"" <> fieldName <> "\".")

instance Unknown InputFieldsDefinition InputContext where
  type UnknownSelector InputFieldsDefinition = ObjectEntry RESOLVED
  unknown Context { scopePosition } ctx _ ObjectEntry { entryName } = 
    [
      GQLError 
        { message = renderInputPrefix ctx <>"Unknown Field \"" <> entryName <> "\"."
        , locations = [scopePosition]
        }
    ]

instance Unknown FieldsDefinition ctx where
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
        <> typeConName <>"\"."
      , locations = [variablePosition]
      }
