{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Api
  ( apiTest,
    assertion,
  )
where

import Control.Applicative (pure)
import Control.Monad (Monad)
import qualified Data.Aeson as A
import Data.Aeson (decode, encode)
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Core (defaultConfig, runApi)
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
  ( assertValidSchema,
    expectedResponse,
    getGQLBody,
    getRequest,
    getResolvers,
    maybeVariables,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )
import Utils.Schema
  ( testSchema,
  )
import Prelude
  ( ($),
    (.),
    (==),
    FilePath,
    IO,
    otherwise,
    show,
  )

assertion :: A.Value -> ResponseStream e Identity (Value VALID) -> IO ()
assertion expected (ResultT (Identity Success {result}))
  | Just expected == decode (encode result) = pure ()
  | otherwise =
    assertFailure $ show ("expected: \n " <> msg expected <> " \n but got: \n " <> msg result)
assertion _ (ResultT (Identity Failure {errors})) = assertFailure (show errors)

apiTest :: FieldName -> TestTree
apiTest path = testCase "TODO: description" $ do
  schema <- assertValidSchema path
  resolvers <- getResolvers path
  actual <- runApi schema resolvers defaultConfig <$> getRequest path
  expected <- expectedResponse path
  assertion expected actual
