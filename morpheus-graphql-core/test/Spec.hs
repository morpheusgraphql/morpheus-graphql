{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main
  ( main,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Core (runApi)
import Data.Morpheus.QuasiQuoter (dsl)
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST (Name, Schema, VALID, Value (..), schemaFromTypeDefinitions)
import Data.Morpheus.Types.Internal.Resolving (Deriving (..), ObjectDeriving (..), ResolverModel (..), ResponseStream, Result (..), ResultT (..))
import Data.Semigroup ((<>))
import qualified Data.Text.Lazy as LT (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Lib (getGQLBody, maybeVariables)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

getSchema :: Applicative m => ResponseStream e m Schema
getSchema =
  schemaFromTypeDefinitions
    [dsl|

  scalar String
  
  type Query {
    deity(name: String): Deity!
  }

  type Deity {
    name: String!
    power: String
  }
|]

encoding :: ResolverModel e m
encoding =
  ResolverModel
    { query = pure $ DerivingObject (ObjectDeriving {__typename = "Query", objectFields = []}),
      mutation = pure DerivingNull,
      subscription = pure DerivingNull
    }

simpleTest :: GQLRequest -> ResponseStream e Identity (Value VALID)
simpleTest request = do
  schema <- getSchema
  runApi schema encoding request

main :: IO ()
main =
  do
    request <- getRequest "simpleQuery"
    defaultMain $
      testGroup
        "core tests"
        [basicTest (simpleTest request)]

expected :: Value VALID
expected = Null

basicTest :: ResponseStream e Identity (Value VALID) -> TestTree
basicTest = testCase "basic test" . assertion

assertion :: ResponseStream e Identity (Value VALID) -> IO ()
assertion (ResultT (Identity Success {result}))
  | expected == result = return ()
  | otherwise =
    assertFailure $
      "expected: \n " <> show expected <> " \n but got: \n " <> show result
assertion (ResultT (Identity Failure {errors})) = assertFailure (show errors)

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
