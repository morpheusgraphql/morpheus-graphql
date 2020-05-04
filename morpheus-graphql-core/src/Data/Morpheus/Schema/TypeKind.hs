{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Schema.TypeKind
  ( TypeKind (..),
  )
where

import Data.Aeson (FromJSON (..))
import GHC.Generics

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
