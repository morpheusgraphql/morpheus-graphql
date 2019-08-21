{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Schema.DirectiveLocation
  ( DirectiveLocation(..)
  ) where

import           Data.Aeson                  (FromJSON (..))
import           Data.Morpheus.Kind          (ENUM)
import           Data.Morpheus.Types.GQLType (GQLType (KIND, __typeName, __typeVisibility))
import           GHC.Generics

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
  deriving (Show, Generic, FromJSON)

instance GQLType DirectiveLocation where
  type KIND DirectiveLocation = ENUM
  __typeName = const "__DirectiveLocation"
  __typeVisibility = const False
