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
    storeSubscriptions,
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

apolloQueryWrapper :: ByteString -> ByteString -> ByteString
apolloQueryWrapper sid query = "{\"id\":\"" <> sid <> "\",\"type\":\"start\",\"payload\":{\"variables\":{},\"operationName\":\"MySubscription\",\"query\":\"" <> query <> "\"}}"

subscriptionQuery :: ByteString
subscriptionQuery = "subscription MySubscription { newDeity { name }}"

simulateStart :: (Input WS -> Stream WS e (SubM e)) -> IO (Input WS, TrailState e)
simulateStart api = do
  (input, TrailState _ o s) <- simulateConnection api
  TrailState _ o' s' <- trail api input (TrailState [apolloQueryWrapper "1" subscriptionQuery] o s)
  state <- trail api input (TrailState [apolloQueryWrapper "5" subscriptionQuery] o' s')
  pure (input, state)

testSubscriptionStart ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testSubscriptionStart api = do
  (input, TrailState {inputs, outputs, store}) <- simulateStart api
  pure $
    testGroup
      "subscription start"
      [ inputsAreConsumed inputs,
        testResponse
          []
          outputs,
        storeSubscriptions
          input
          ["1", "5"]
          store
      ]

stopSubscription :: ByteString -> ByteString
stopSubscription x = "{\"id\":\"" <> x <> "\",\"type\":\"stop\"}"

testSubscriptionStop ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testSubscriptionStop api = do
  (input, TrailState _ o s) <- simulateStart api
  TrailState {inputs, outputs, store} <- trail api input (TrailState [stopSubscription "1"] o s)
  pure $
    testGroup
      "stop subscription"
      [ inputsAreConsumed inputs,
        testResponse
          []
          outputs,
        storeSubscriptions
          input
          ["5"]
          store
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
  start <- testSubscriptionStart api
  stop <- testSubscriptionStop api
  return $ testGroup "ApolloRequest" [unknownType, connection_init, start, stop]
