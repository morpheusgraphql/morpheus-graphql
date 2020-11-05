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
{-# LANGUAGE NoImplicitPrelude #-}

module Rendering.Schema
  ( proxy,
    path,
  )
where

import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types
  ( GQLScalar (..),
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

instance GQLScalar TestScalar where
  parseValue _ = pure TestScalar
  serialize TestScalar = Int 0

importGQLDocumentWithNamespace "test/Rendering/schema.gql"

path :: String
path = "test/Rendering/schema.gql"

proxy :: Proxy (RootResolver IO () MyQuery MyMutation Undefined)
proxy = Proxy @(RootResolver IO () MyQuery MyMutation Undefined)
