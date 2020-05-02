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

module Subscription.API (api, EVENT, Channel (..), Info (..)) where

import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
  ( Event (..),
    GQLRootResolver (..),
    Input,
    Stream,
    subscribe,
  )
import Data.Text (Text)
import Subscription.Utils (SubM)

data Channel
  = DEITY
  | HUMAN
  deriving (Show, Eq)

importGQLDocument "test/Subscription/schema.gql"

type EVENT = Event Channel Info

character :: Applicative m => m (Character m)
character =
  pure
    Character
      { name = pure "testName",
        age = pure 1
      }

characterSub :: Applicative m => EVENT -> m (Character m)
characterSub (Event _ Info {name, age}) =
  pure
    Character
      { name = pure name,
        age = pure age
      }

rootResolver :: GQLRootResolver (SubM EVENT) EVENT Query Mutation Subscription
rootResolver =
  GQLRootResolver
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
          { newDeity = subscribe [DEITY] (pure characterSub),
            newHuman = subscribe [HUMAN] (pure characterSub)
          }
    }

api :: Input api -> Stream api EVENT (SubM EVENT)
api = interpreter rootResolver
