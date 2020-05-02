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

simulateConnection :: (Input WS -> Stream WS e (SubM e)) -> IO (Input WS, TrailState e)
simulateConnection api = do
  input <- connect
  state <- trail api input (TrailState ["{ \"type\":\"connection_init\" }"] [] empty)
  pure (input, state)

testConnectionInit ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testConnectionInit api = do
  (input, TrailState {inputs, outputs, store}) <- simulateConnection api
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

apolloQueryWrapper :: ByteString -> ByteString
apolloQueryWrapper query = "{\"id\":\"1\",\"type\":\"start\",\"payload\":{\"variables\":{},\"operationName\":\"MySubscription\",\"query\":\"" <> query <> "\"}}"

subscriptionQuery :: ByteString
subscriptionQuery = "subscription MySubscription { newDeity { name }}"

simulateStart :: (Input WS -> Stream WS e (SubM e)) -> IO (Input WS, TrailState e)
simulateStart api = do
  (input, TrailState _ o s) <- simulateConnection api
  state <- trail api input (TrailState [apolloQueryWrapper subscriptionQuery] o s)
  pure (input, state)

testSubscriptionStart ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testSubscriptionStart api = do
  (_, TrailState {inputs, outputs, store}) <- simulateStart api
  pure $
    testGroup
      "subscription start"
      [ inputsAreConsumed inputs,
        testResponse [] outputs
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
  subscriptionStart <- testSubscriptionStart api
  return $ testGroup "ApolloRequest" [unknownType, connection_init, subscriptionStart]
