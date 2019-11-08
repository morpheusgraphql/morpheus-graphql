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

module Feature.Holistic.API
  ( api
  , rootResolver
  )
where

import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocument )
import           Data.Morpheus.Kind             ( SCALAR )
import           Data.Morpheus.Types            ( Event
                                                , GQLRequest
                                                , GQLResponse
                                                , GQLRootResolver(..)
                                                , GQLScalar(..)
                                                , GQLType(..)
                                                , ID(..)
                                                , ScalarValue(..)
                                                , Resolver(..)
                                                , constRes
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


data TestScalar =
  TestScalar Int
             Int
  deriving (Show, Generic)

instance GQLType TestScalar where
  type KIND TestScalar = SCALAR

instance GQLScalar TestScalar where
  parseValue _ = pure (TestScalar 1 0)
  serialize (TestScalar x y) = Int (x * 100 + y)

data Channel =
  Channel
  deriving (Show, Eq)

type EVENT = Event Channel ()

importGQLDocument "test/Feature/Holistic/schema.gql"

rootResolver :: GQLRootResolver IO EVENT Query Mutation Subscription
rootResolver = GQLRootResolver
  { queryResolver        = Query { user, testUnion = constRes Nothing }
  , mutationResolver     = Mutation { createUser = user }
  , subscriptionResolver = Subscription
                             { newUser = const SubResolver
                                           { subChannels = [Channel]
                                           , subResolver = user
                                           }
                             }
  }
 where
  user _ = pure User { name    = constRes "testName"
                     , email   = constRes ""
                     , address = resolveAddress
                     , office  = resolveAddress
                     , friend  = constRes Nothing
                     }
   where
    resolveAddress _ = pure Address { city        = constRes ""
                                    , houseNumber = constRes 0
                                    , street      = constRes Nothing
                                    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
