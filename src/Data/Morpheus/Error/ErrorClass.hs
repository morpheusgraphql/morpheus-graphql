{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Morpheus.Error.ErrorClass
  ( MissingRequired(..)
  , KindViolation(..)
  , Unknown(..)
  , InternalError(..)
  )
  where

import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Error.Selection  ( unknownSelectionField
                                                , hasNoSubfields
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( Name
                                                , RESOLVED
                                                , Ref(..)
                                                , TypeRef(..)
                                                , GQLError(..)
                                                , GQLErrors
                                                , Argument(..)
                                                , ObjectEntry(..)
                                                , Fragment(..)
                                                , Fragments
                                                , ValidationContext(..)
                                                , Variable(..)
                                                , VariableDefinitions
                                                , FieldDefinition(..)
                                                , FieldsDefinition
                                                , Schema
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

instance InternalError (a, (Ref, Name)) where
  


class MissingRequired c where 
  missingRequired :: ValidationContext -> Ref -> c -> GQLError

instance MissingRequired (VariableDefinitions s) where
  missingRequired 
    ValidationContext { operationName } 
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
  unknown :: c -> UnknownSelector c -> GQLErrors

-- {...H} -> "Unknown fragment \"H\"."
instance Unknown Fragments where
  type UnknownSelector Fragments = Ref
  unknown _ (Ref name pos) 
    = errorMessage pos
      ("Unknown Fragment \"" <> name <> "\".")

instance Unknown Schema where
  type UnknownSelector Schema = Ref
  unknown _ Ref { refName , refPosition }
    = errorMessage refPosition ("Unknown type \"" <> refName <> "\".")

instance Unknown FieldDefinition where
  type UnknownSelector FieldDefinition = Argument RESOLVED
  unknown FieldDefinition { fieldName } Argument { argumentName, argumentPosition }
    = errorMessage argumentPosition 
      ("Unknown Argument \"" <> argumentName <> "\" on Field \"" <> fieldName <> "\".")

instance Unknown FieldsDefinition where
  type UnknownSelector FieldsDefinition = ObjectEntry RESOLVED
  unknown  _ ObjectEntry { entryName } = 
    [
      GQLError 
        { message = "Unknown Field \"" <> entryName <> "\"."
        , locations = []
        }
    ]

class KindViolation a where
  kindViolation :: a -> GQLError

instance KindViolation Fragment where
  kindViolation Fragment { fragmentName, fragmentType, fragmentPosition } 
    = GQLError
    { message   
      = "Fragment \"" <> fragmentName 
        <> "\" cannot condition on non composite type \"" 
        <> fragmentType <>"\"."
    , locations = [fragmentPosition]
    }

instance KindViolation (Variable s) where
  kindViolation Variable 
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

instance KindViolation (Ref, Name) where
  -- kindViolation (ref,name) = head $ hasNoSubfields ref name