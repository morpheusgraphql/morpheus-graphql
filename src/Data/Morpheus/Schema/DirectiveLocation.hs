{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Schema.DirectiveLocation
  ( DirectiveLocation(..)
  ) where

import           Data.Morpheus.Kind          (ENUM, KIND)
import           Data.Morpheus.Types.GQLType (GQLType (__typeName))
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
  | VARIABLE_DEFINITION
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

instance GQLType DirectiveLocation where
  __typeName = const "__DirectiveLocation"
