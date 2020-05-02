{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.Case.Publishing
  ( testPublishing,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Types
  ( Input,
    Stream,
  )
import Data.Morpheus.Types (Event (..))
import Data.Morpheus.Types.Internal.Subscription
  ( WS,
    connect,
    empty,
  )
import Data.Semigroup ((<>))
import Subscription.API (Channel (..), EVENT, api)
import Subscription.Utils
  ( SubM,
    TrailState (..),
    inputsAreConsumed,
    simulatePublish,
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

simulateConnection :: (Input WS -> Stream WS e (SubM e)) -> IO (Input WS, TrailState e)
simulateConnection api = do
  input <- connect
  state <- trail api input (TrailState ["{ \"type\":\"connection_init\" }"] [] empty)
  pure (input, state)

apolloQueryWrapper :: ByteString -> ByteString -> ByteString
apolloQueryWrapper query sid = "{\"id\":\"" <> sid <> "\",\"type\":\"start\",\"payload\":{\"variables\":{},\"operationName\":\"MySubscription\",\"query\":\"" <> query <> "\"}}"

apolloResWrapper :: ByteString -> ByteString -> ByteString
apolloResWrapper sid value = "{\"id\":\"" <> sid <> "\",\"type\":\"data\",\"payload\":{\"data\":" <> value <> "}}"

subscriptionQuery :: ByteString
subscriptionQuery = "subscription MySubscription { newDeity { name }}"

apolloStart :: ByteString -> ByteString
apolloStart = apolloQueryWrapper subscriptionQuery

stopSubscription :: ByteString -> ByteString
stopSubscription x = "{\"id\":\"" <> x <> "\",\"type\":\"stop\"}"

simulateSubscriptions :: IO (Input WS, TrailState EVENT)
simulateSubscriptions = do
  (input, TrailState _ o s) <- simulateConnection api
  state <-
    trail api input (TrailState [apolloStart "1", apolloStart "5", stopSubscription "1"] o s)
      -- execute apolloStart "5"
      >>= trail api input
      -- execute stopSubscription "1"
      >>= trail api input
  pure (input, state)

triggerSubsciption ::
  IO TestTree
triggerSubsciption = do
  (input, state) <- simulateSubscriptions
  TrailState {inputs, outputs, store} <- simulatePublish (Event [Channel] ()) state
  pure $
    testGroup
      "publish event"
      [ inputsAreConsumed inputs,
        testResponse
          [ apolloResWrapper
              "5"
              "{\"newDeity\":{\"name\":\"testName\"}}"
          ]
          outputs,
        storeSubscriptions
          input
          ["5"]
          store
      ]

testPublishing :: IO TestTree
testPublishing = do
  trigger <- triggerSubsciption
  return $ testGroup "Publishing" [trigger]
