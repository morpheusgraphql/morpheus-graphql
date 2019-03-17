{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Data.Morpheus.Schema.TypeKind
  ( TypeKind(..)
  ) where

import           Data.Data    (Data)
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
  deriving (Show, Data, Generic)
