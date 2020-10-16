{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Morpheus.Client.JSONSchema.TypeRef
  ( TypeRef (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Morpheus.Types.Internal.AST
  ( TypeName,
  )
import Relude

newtype TypeRef = TypeRef {name :: TypeName}
  deriving (Generic, Show, FromJSON)
