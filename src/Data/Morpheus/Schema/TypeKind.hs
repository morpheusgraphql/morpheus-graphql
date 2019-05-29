{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Morpheus.Schema.TypeKind
  ( TypeKind(..)
  ) where

import           Data.Morpheus.Kind.Internal (ENUM, KIND)
import           GHC.Generics

type instance KIND TypeKind = ENUM

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
