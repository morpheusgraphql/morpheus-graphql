{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Rendering.Schema
  ( proxy,
    path,
  )
where

import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
  ( GQLScalar (..),
    GQLType (..),
    ID (..),
    RootResolver (..),
    ScalarValue (..),
    Undefined (..),
  )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data TestScalar
  = TestScalar
  deriving (Show, Generic)

instance GQLType TestScalar where
  type KIND TestScalar = SCALAR

instance GQLScalar TestScalar where
  parseValue _ = pure TestScalar
  serialize TestScalar = Int 0

importGQLDocumentWithNamespace "test/Rendering/schema.gql"

path :: String
path = "test/Rendering/schema.gql"

proxy :: Proxy (RootResolver IO () Query Undefined Undefined)
proxy = Proxy @(RootResolver IO () Query Undefined Undefined)
