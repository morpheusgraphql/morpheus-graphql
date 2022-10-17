{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch.Types
  ( FetchError (..),
    SchemaSource (..),
    ExecutableSource,
    GQLClientResult,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
  )
import Relude hiding (ByteString)

data FetchError a
  = FetchErrorParseFailure String
  | FetchErrorProducedErrors GQLErrors (Maybe a)
  | FetchErrorNoResult
  deriving (Show, Eq, Generic)

data SchemaSource
  = JSON ByteString
  | GQL ByteString
  deriving (Show, Eq)

type ExecutableSource = Text

type GQLClientResult (a :: Type) = (Either (FetchError a) a)
