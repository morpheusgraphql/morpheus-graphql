{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.Case.ApolloRequest
  ( testApolloRequest,
    SubM,
  )
where

-- import qualified Data.Text.Lazy             as LT (toStrict)
-- import           Data.Text.Lazy.Encoding    (decodeUtf8)
import Data.ByteString.Lazy.Char8 (ByteString)
-- import qualified Data.ByteString.Lazy.Char8 as LB
--   ( unpack,
--   )
import Data.Morpheus.Types
  ( Input,
    Stream,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( GQLChannel (..),
    WS,
    connect,
    empty,
  )
import Data.Semigroup ((<>))
import Subscription.Utils
  ( SubM,
    TrailState (..),
    expectedResponse,
    inputsAreConsumed,
    storeIsEmpty,
    stored,
    storedSingle,
    testResponse,
    trail,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

testUnknownType ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testUnknownType api = do
  input <- connect
  TrailState {inputs, outputs, store} <- trail api input (TrailState ["{ \"type\":\"bla\" }"] [] empty)
  pure $
    testGroup
      "unknown request type"
      [ inputsAreConsumed inputs,
        testResponse
          ["Unknown Request type \"bla\"."]
          outputs,
        storeIsEmpty store
      ]

testConnectionInit ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testConnectionInit api = do
  input <- connect
  TrailState {inputs, outputs, store} <- trail api input (TrailState ["{ \"type\":\"connection_init\" }"] [] empty)
  pure $
    testGroup
      "connection init"
      [ inputsAreConsumed inputs,
        testResponse
          []
          outputs,
        stored input store,
        storedSingle store
      ]

testApolloRequest ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testApolloRequest api = do
  unknownType <- testUnknownType api
  connection_init <- testConnectionInit api
  return $ testGroup "ApolloRequest" [unknownType, connection_init]
