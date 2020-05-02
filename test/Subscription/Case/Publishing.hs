{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.Case.Publishing
  ( testPublishing,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Types
  ( Event (..),
    Input,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( WS,
    connect,
    empty,
  )
import Subscription.API
  ( Channel (..),
    EVENT,
    Info (..),
    api,
  )
import Subscription.Utils
  ( SimulationState (..),
    apolloInit,
    apolloRes,
    apolloStart,
    apolloStop,
    inputsAreConsumed,
    simulateAll,
    simulatePublish,
    storeSubscriptions,
    testResponse,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )

startNewDeity :: ByteString -> ByteString
startNewDeity = apolloStart "subscription MySubscription { newDeity { name , age }}"

startNewHuman :: ByteString -> ByteString
startNewHuman = apolloStart "subscription MySubscription { newHuman { name , age }}"

simulateSubscriptions :: IO (Input WS, SimulationState EVENT)
simulateSubscriptions = do
  input <- connect
  state <-
    simulateAll
      api
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

triggerSubsciption ::
  IO TestTree
triggerSubsciption = do
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
          [ apolloRes
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
  trigger <- triggerSubsciption
  return $ testGroup "Publishing" [trigger]
