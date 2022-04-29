{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Schema.JSON.TypeRef
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
