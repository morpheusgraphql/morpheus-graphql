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

module Subscription.API (api, EVENT, Channel (..)) where

import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
  ( Event,
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

type EVENT = Event Channel ()

importGQLDocument "test/Subscription/schema.gql"

human :: Applicative m => m (Human m)
human =
  pure
    Human
      { name = pure "testName",
        age = pure 1
      }

deity :: Applicative m => m (Deity m)
deity =
  pure
    Deity
      { name = pure "testName",
        age = pure 1
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
          { createDeity = const deity,
            createHuman = const human
          },
      subscriptionResolver =
        Subscription
          { newDeity = subscribe [DEITY] (pure $ const deity),
            newHuman = subscribe [HUMAN] (pure $ const human)
          }
    }

api :: Input api -> Stream api EVENT (SubM EVENT)
api = interpreter rootResolver
