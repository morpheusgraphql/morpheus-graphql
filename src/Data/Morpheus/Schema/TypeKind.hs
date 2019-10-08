{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Schema.TypeKind
  ( TypeKind(..)
  ) where

import           Data.Aeson                  (FromJSON (..))
import           Data.Morpheus.Kind          (ENUM)
import           Data.Morpheus.Types.GQLType (GQLType (KIND, __typeName))
import           GHC.Generics

instance GQLType TypeKind where
  type KIND TypeKind = ENUM
  __typeName = const "__TypeKind"

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
