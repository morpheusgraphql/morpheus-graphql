{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Subscription.API
  ( app,
    EVENT,
    Channel (..),
    Info (..),
  )
where

import Data.Morpheus (App, deriveApp)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Subscriptions
  ( Event (..),
  )
import Data.Morpheus.Types
  ( RootResolver (..),
    subscribe,
  )
import Relude
import Subscription.Utils (SubM)

data Channel
  = DEITY
  | HUMAN
  deriving
    ( Show,
      Eq,
      Generic,
      Hashable
    )

importGQLDocument "test/Subscription/schema.gql"

type EVENT = Event Channel Info

character :: (Applicative m) => m (Character m)
character =
  pure
    Character
      { name = pure "testName",
        age = pure 1
      }

characterSub :: (Applicative m) => EVENT -> m (Character m)
characterSub (Event _ Info {name, age}) =
  pure
    Character
      { name = pure name,
        age = pure age
      }

root :: RootResolver (SubM EVENT) EVENT Query Mutation Subscription
root =
  RootResolver
    { queryResolver =
        Query
          { queryField = pure ""
          },
      mutationResolver =
        Mutation
          { createDeity = const character,
            createHuman = const character
          },
      subscriptionResolver =
        Subscription
          { newDeity = subscribe DEITY (pure characterSub),
            newHuman = subscribe HUMAN (pure characterSub)
          }
    }

app :: App EVENT (SubM EVENT)
app = deriveApp root
