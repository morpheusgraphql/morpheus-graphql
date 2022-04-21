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
    ID,
    RootResolver,
    Undefined,
  )
import Relude hiding (Undefined)

data TestScalar = TestScalar
  deriving (Show)

instance DecodeScalar TestScalar where
  decodeScalar _ = pure TestScalar

importGQLDocumentWithNamespace "test/Rendering/schema.gql"

type APIResolver e (m :: Type -> Type) =
  RootResolver m e MyQuery MyMutation Undefined

proxy :: Proxy (APIResolver () IO)
proxy = Proxy
