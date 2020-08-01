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
    assertion,
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
    uncurry,
  )

apiSchema :: Schema VALID
apiSchema =
  [dsl|

  type Query {
    deity(name: String): Deity!
  }

  interface Character {
    name : String
  }

  interface Supernatural {
    power: [String!]!
  }

  type Hero implements Character {
    name: String
  }

  type Deity implements Character Supernatural {
    name: String!
    power: [String!]!
  }
|]

resolver :: Monad m => RootResModel e m
resolver =
  RootResModel
    { query =
        pure $
          mkObject
            "Query"
            [("deity", resolveDeity)],
      mutation = pure mkNull,
      subscription = pure mkNull,
      channelMap = Nothing
    }

resolveDeity :: (WithOperation o, Monad m) => Resolver o e m (ResModel o e m)
resolveDeity =
  pure $
    mkObject
      "Deity"
      [ ("name", pure $ mkString "Morpheus"),
        ("power", pure $ mkList [mkString "Shapeshifting"])
      ]

main :: IO ()
main = do
  schema <- testSchema
  defaultMain
    $ testGroup
      "core tests"
    $ fmap
      (uncurry basicTest)
      [ ("basic Test", "api/simple"),
        ("test interface", "api/interface")
      ]
      <> [ schema,
           apiTest "/api/validation/simple"
         ]

basicTest :: FilePath -> FieldName -> TestTree
basicTest description path = testCase description $ do
  actual <- simpleTest <$> getRequest path
  expected <- expectedResponse path
  assertion expected actual

simpleTest :: GQLRequest -> ResponseStream e Identity (Value VALID)
simpleTest = runApi apiSchema resolver defaultConfig
