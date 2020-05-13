{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main
  ( main,
  )
where

import qualified Data.Aeson as A
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Core (runApi)
import Data.Morpheus.QuasiQuoter (dsl)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Name,
    Schema,
    VALID,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Operation
  ( fromList,
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
  ( getGQLBody,
    getResponseBody,
    maybeVariables,
  )
import Schema
  ( testSchema,
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

getSchema :: Monad m => ResponseStream e m Schema
getSchema =
  fromList
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
      subscription = pure mkNull
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
main =
  defaultMain
    $ testGroup
      "core tests"
    $ map
      (uncurry basicTest)
      [ ("basic Test", "simple"),
        ("test interface", "interface")
      ]
      <> [testSchema]

basicTest :: String -> Name -> TestTree
basicTest description path = testCase description $ do
  actual <- simpleTest <$> getRequest path
  expected <- expectedResponse path
  assertion expected actual

simpleTest :: GQLRequest -> ResponseStream e Identity (Value VALID)
simpleTest request = do
  schema <- getSchema
  runApi schema resolver request

expectedResponse :: Name -> IO A.Value
expectedResponse = getResponseBody

assertion :: A.Value -> ResponseStream e Identity (Value VALID) -> IO ()
assertion expected (ResultT (Identity Success {result}))
  | Just expected == decode (encode result) = return ()
  | otherwise =
    assertFailure $ show ("expected: \n " <> msg expected <> " \n but got: \n " <> msg result)
assertion _ (ResultT (Identity Failure {errors})) = assertFailure (show errors)

getRequest :: Name -> IO GQLRequest
getRequest path = do
  queryBS <- LT.toStrict . decodeUtf8 <$> getGQLBody path
  variables <- maybeVariables path
  pure $
    GQLRequest
      { operationName = Nothing,
        query = queryBS,
        variables
      }
