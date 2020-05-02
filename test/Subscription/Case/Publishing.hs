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

startNewDeity :: ByteString -> ByteString
startNewDeity = apolloStart "subscription MySubscription { newDeity { name }}"

startNewHuman :: ByteString -> ByteString
startNewHuman = apolloStart "subscription MySubscription { newHuman { name }}"

simulateSubscriptions :: IO (Input WS, TrailState EVENT)
simulateSubscriptions = do
  input <- connect
  state <-
    trail
      api
      input
      ( TrailState
          [ apolloInit,
            startNewDeity "1",
            startNewDeity "2",
            startNewDeity "3",
            startNewHuman "4",
            apolloStop "1"
          ]
          []
          empty
      )
      -- execute startNewDeity "1"
      >>= trail api input
      -- execute startNewDeity "2"
      >>= trail api input
      -- execute startNewDeity "3"
      >>= trail api input
      -- execute startNewHuman "4"
      >>= trail api input
      -- execute stopSubscription "1"
      >>= trail api input
  pure (input, state)

triggerSubsciption ::
  IO TestTree
triggerSubsciption = do
  (input, state) <- simulateSubscriptions
  TrailState {inputs, outputs, store} <-
    simulatePublish (Event [DEITY] ()) state
      >>= simulatePublish (Event [HUMAN] ())
  pure $
    testGroup
      "publish event"
      [ inputsAreConsumed inputs,
        testResponse
          -- triggers subscriptions by channels
          [ apolloRes
              "2"
              "{\"newDeity\":{\"name\":\"testName\"}}",
            apolloRes
              "3"
              "{\"newDeity\":{\"name\":\"testName\"}}",
            apolloRes
              "4"
              "{\"newHuman\":{\"name\":\"testName\"}}"
          ]
          outputs,
        storeSubscriptions
          input
          ["2", "3", "4"]
          store
      ]

testPublishing :: IO TestTree
testPublishing = do
  trigger <- triggerSubsciption
  return $ testGroup "Publishing" [trigger]
