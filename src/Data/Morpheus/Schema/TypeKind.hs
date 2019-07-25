{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Schema.TypeKind
  ( TypeKind(..)
  ) where

import           Data.Morpheus.Kind          (ENUM, KIND)
import           Data.Morpheus.Types.GQLType (GQLType (__typeName, __typeVisibility))
import           GHC.Generics

type instance KIND TypeKind = ENUM

instance GQLType TypeKind where
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
