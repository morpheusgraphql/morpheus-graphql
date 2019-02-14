{-# LANGUAGE DeriveGeneric, DeriveDataTypeable  ,  DeriveAnyClass #-}

module Data.Morpheus.Schema.GQL__DirectiveLocation
  (GQL__DirectiveLocation(..))
where

import           GHC.Generics
import           Data.Aeson                     ( ToJSON(..) )
import           Data.Data                      ( Data )

data GQL__DirectiveLocation =
   QUERY
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
  | INPUT_FIELD_DEFINITION deriving (Show , Data, Generic, ToJSON )
