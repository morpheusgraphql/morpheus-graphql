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
import Data.Morpheus.Types
  ( ID (..),
    RootResolver (..),
    ScalarDecoder (..),
    Undefined (..),
  )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data TestScalar
  = TestScalar
  deriving (Show, Generic)

instance ScalarDecoder TestScalar where
  parseValue _ = pure TestScalar

importGQLDocumentWithNamespace "test/Rendering/schema.gql"

path :: String
path = "test/Rendering/schema.gql"

proxy :: Proxy (RootResolver IO () MyQuery MyMutation Undefined)
proxy = Proxy @(RootResolver IO () MyQuery MyMutation Undefined)
