{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.Case.Publishing
  ( testPublishing,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Types
  ( Event (..),
    Input,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( WS,
    connect,
    empty,
  )
import Subscription.API (Channel (..), EVENT, api)
import Subscription.Utils
  ( TrailState (..),
    apolloInit,
    apolloRes,
    apolloStart,
    apolloStop,
    inputsAreConsumed,
    simulatePublish,
    storeSubscriptions,
    testResponse,
    trail,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )

start :: ByteString -> ByteString
start = apolloStart "subscription MySubscription { newDeity { name }}"

simulateSubscriptions :: IO (Input WS, TrailState EVENT)
simulateSubscriptions = do
  input <- connect
  state <-
    trail api input (TrailState [apolloInit, start "1", start "5", apolloStop "1"] [] empty)
      -- execute apolloStart "1"
      >>= trail api input
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
          [ apolloRes
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
