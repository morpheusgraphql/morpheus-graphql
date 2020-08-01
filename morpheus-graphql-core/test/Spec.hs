{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Control.Applicative (pure)
import Control.Monad (Monad)
import qualified Data.Aeson as A
import Data.Aeson (decode, encode)
import Data.Functor ((<$>), fmap)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Core (defaultConfig, runApi)
import Data.Morpheus.QuasiQuoter (dsl)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    Schema,
    VALID,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ResModel,
    Resolver,
    ResponseStream,
    Result (..),
    ResultT (..),
    RootResModel (..),
    WithOperation,
    mkList,
    mkNull,
    mkObject,
    mkString,
  )
import Data.Semigroup ((<>))
import qualified Data.Text.Lazy as LT (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Lib
  ( expectedResponse,
    getGQLBody,
    getRequest,
    maybeVariables,
  )
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )
import Utils.Api
  ( apiTest,
  )
import Utils.Schema
  ( testSchema,
  )
import Prelude
  ( ($),
    IO,
  )

main :: IO ()
main = do
  schema <- testSchema
  defaultMain $
    testGroup
      "core tests"
      [ schema,
        apiTest "api" ["simple", "interface"]
      ]
