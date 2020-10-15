{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Client.JSONSchema.TypeKind
  ( TypeKind (..),
  )
where

import Data.Aeson (FromJSON (..))
import GHC.Generics
import Relude

data TypeKind
  = SCALAR
  | OBJECT
  | INTERFACE
  | UNION
  | ENUM
  | INPUT_OBJECT
  | LIST
  | NON_NULL
  deriving (Eq, Generic, FromJSON, Show)
