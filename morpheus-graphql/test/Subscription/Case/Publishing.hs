{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Subscription.Case.Publishing
  ( testPublishing,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Subscriptions
  ( Event (..),
  )
import Data.Morpheus.Subscriptions.Internal
  ( Input,
    SUB,
    connect,
    empty,
  )
import Subscription.API
  ( Channel (..),
    EVENT,
    Info (..),
    app,
  )
import Subscription.Utils
  ( SimulationState (..),
    apolloConnectionAck,
    apolloInit,
    apolloRes,
    apolloStart,
    apolloStop,
    inputsAreConsumed,
    simulate,
    simulatePublish,
    storeSubscriptions,
    testResponse,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Prelude

startNewDeity :: ByteString -> ByteString
startNewDeity = apolloStart "subscription MySubscription { newDeity { name , age }}"

startNewHuman :: ByteString -> ByteString
startNewHuman = apolloStart "subscription MySubscription { newHuman { name , age }}"

simulateSubscriptions :: IO (Input SUB, SimulationState EVENT)
simulateSubscriptions = do
  input <- connect
  state <-
    simulate
      app
      input
      ( SimulationState
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
  pure (input, state)

triggerSubscription ::
  IO TestTree
triggerSubscription = do
  (input, state) <- simulateSubscriptions
  SimulationState {inputs, outputs, store} <-
    simulatePublish (Event [DEITY] Info {name = "Zeus", age = 1200}) state
      >>= simulatePublish (Event [HUMAN] Info {name = "Hercules", age = 18})
  pure $
    testGroup
      "publish event"
      [ inputsAreConsumed inputs,
        testResponse
          -- triggers subscriptions by channels
          [ apolloConnectionAck,
            apolloRes
              "2"
              "{\"newDeity\":{\"name\":\"Zeus\",\"age\":1200}}",
            apolloRes
              "3"
              "{\"newDeity\":{\"name\":\"Zeus\",\"age\":1200}}",
            apolloRes
              "4"
              "{\"newHuman\":{\"name\":\"Hercules\",\"age\":18}}"
          ]
          outputs,
        storeSubscriptions
          input
          ["2", "3", "4"]
          store
      ]

testPublishing :: IO TestTree
testPublishing = do
  trigger <- triggerSubscription
  return $ testGroup "Publishing" [trigger]
