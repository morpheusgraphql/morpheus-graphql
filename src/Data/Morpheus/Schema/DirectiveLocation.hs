{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Morpheus.Schema.DirectiveLocation
  ( DirectiveLocation(..)
  ) where

import           Data.Morpheus.Kind (ENUM, KIND)
import           GHC.Generics

type instance KIND DirectiveLocation = ENUM

data DirectiveLocation
  = QUERY
  | MUTATION
  | SUBSCRIPTION
  | FIELD
  | FRAGMENT_DEFINITION
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT
  | SCHEMA
  | SCALAR
  | OBJECT
  | FIELD_DEFINITION
  | ARGUMENT_DEFINITION
  | INTERFACE
  | UNION
  | ENUM
  | ENUM_VALUE
  | INPUT_OBJECT
  | INPUT_FIELD_DEFINITION
  deriving (Generic)
