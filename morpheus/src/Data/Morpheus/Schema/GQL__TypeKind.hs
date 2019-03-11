{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Data.Morpheus.Schema.GQL__TypeKind
  ( GQL__TypeKind(..)
  ) where

import           Data.Aeson   (ToJSON (..))
import           Data.Data    (Data)
import           GHC.Generics

data GQL__TypeKind
  = SCALAR
  | OBJECT
  | INTERFACE
  | UNION
  | ENUM
  | INPUT_OBJECT
  | LIST
  | NON_NULL
  deriving (Show, Data, Generic)
