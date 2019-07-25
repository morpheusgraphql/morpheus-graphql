{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Schema.TypeKind
  ( TypeKind(..)
  ) where

import           Data.Morpheus.Kind          (ENUM)
import           Data.Morpheus.Types.GQLType (GQLType (KIND, __typeName, __typeVisibility))
import           GHC.Generics

instance GQLType TypeKind where
  type KIND TypeKind = ENUM
  __typeName = const "__TypeKind"
  __typeVisibility = const False

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
