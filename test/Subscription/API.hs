{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Subscription.API
  ( api )
where

import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocument )
import           Data.Morpheus.Types            ( Event
                                                , GQLRootResolver(..)
                                                , Input
                                                , Stream
                                                , subscribe
                                                )
import           Data.Text                      ( Text )

data Channel =
  Channel
  deriving (Show, Eq)

type EVENT = Event Channel ()

importGQLDocument "test/Subscription/schema.gql"

human :: Applicative m => m (Human m)
human 
  = pure 
    Human 
      { name = pure "testName"
      , age   = pure 1
      }

deity :: Applicative m => m (Deity m)
deity 
  = pure 
    Deity 
      { name = pure "testName"
      , age   = pure 1
      }

rootResolver :: GQLRootResolver IO EVENT Query Mutation Subscription
rootResolver = GQLRootResolver
  { queryResolver
      = Query 
        { queryField = pure "" 
        }
  , mutationResolver     
      = Mutation 
        { createDeity = const deity 
        , createHuman = const human
        }
  , subscriptionResolver 
      = Subscription 
        { newDeity = subscribe [Channel] (pure $ const deity)
        , newHuman = subscribe [Channel] (pure $ const human)
        }
  }

api :: Input api -> Stream api EVENT IO
api = interpreter rootResolver