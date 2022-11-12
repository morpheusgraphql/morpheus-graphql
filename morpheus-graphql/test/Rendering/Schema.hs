{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
  )
where

import Data.Morpheus.Document
  ( importGQLDocumentWithNamespace,
  )
import Data.Morpheus.Types
  ( DecodeScalar (..),
    DefaultValue (..),
    DropNamespace (..),
    EncodeScalar (..),
    ID,
    Rename (..),
    RootResolver,
    ScalarValue (..),
    Undefined,
    VisitType,
  )
import Relude hiding (Undefined)

data TestScalar = TestScalar deriving (Show)

instance DecodeScalar TestScalar where
  decodeScalar _ = pure TestScalar

instance EncodeScalar TestScalar where
  encodeScalar _ = String "TestScalar"

importGQLDocumentWithNamespace "test/Rendering/schema.gql"

instance VisitType TestDirective

type APIResolver e (m :: Type -> Type) =
  RootResolver m e MyQuery MyMutation Undefined

proxy :: Proxy (APIResolver () IO)
proxy = Proxy
