{-# LANGUAGE DeriveGeneric #-}

module Data.Morpheus.Schema.TypeKind
  ( TypeKind(..)
  ) where

import           GHC.Generics

data TypeKind
  = SCALAR
  | OBJECT
  | INTERFACE
  | UNION
  | ENUM
  | INPUT_OBJECT
  | LIST
  | NON_NULL
  deriving (Eq, Generic, Show)
