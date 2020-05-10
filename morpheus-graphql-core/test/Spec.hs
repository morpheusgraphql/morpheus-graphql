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
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST (Name, ScalarValue (..), Schema, VALID, Value (..))
import Data.Morpheus.Types.Internal.Operation (fromList)
import Data.Morpheus.Types.Internal.Resolving (ObjectResModel (..), ResModel (..), ResponseStream, Result (..), ResultT (..), RootResModel (..))
import Data.Semigroup ((<>))
import qualified Data.Text.Lazy as LT (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Lib (getGQLBody, getResponseBody, maybeVariables)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

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

string :: Name -> ResModel o e m
string = ResScalar . String

resolver :: Monad m => RootResModel e m
resolver =
  RootResModel
    { query =
        pure $
          ResObject
            ( ObjectResModel
                { __typename = "Query",
                  objectFields =
                    [ ( "deity",
                        pure
                          $ ResObject
                          $ ObjectResModel
                            { __typename = "Deity",
                              objectFields =
                                [ ( "name",
                                    pure $ string "Morpheus"
                                  ),
                                  ( "power",
                                    pure $
                                      ResList
                                        [string "Shapeshifting"]
                                  )
                                ]
                            }
                      )
                    ]
                }
            ),
      mutation = pure ResNull,
      subscription = pure ResNull
    }

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
    assertFailure $
      LB.unpack ("expected: \n " <> encode expected <> " \n but got: \n " <> encode result)
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
