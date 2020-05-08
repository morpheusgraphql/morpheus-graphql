{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Holistic.API
  ( api,
    rootResolver,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
  ( Event,
    GQLRequest,
    GQLResponse,
    GQLRootResolver (..),
    GQLScalar (..),
    GQLType (..),
    ID (..),
    ScalarValue (..),
    liftEither,
    subscribe,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data TestScalar
  = TestScalar
      Int
      Int
  deriving (Show, Generic)

instance GQLType TestScalar where
  type KIND TestScalar = SCALAR

instance GQLScalar TestScalar where
  parseValue _ = pure (TestScalar 1 0)
  serialize (TestScalar x y) = Int (x * 100 + y)

data Channel
  = Channel
  deriving (Show, Eq)

type EVENT = Event Channel ()

importGQLDocument "test/Feature/Holistic/schema.gql"

alwaysFail :: IO (Either String a)
alwaysFail = pure $ Left "fail with Either"

rootResolver :: GQLRootResolver IO EVENT Query Mutation Subscription
rootResolver =
  GQLRootResolver
    { queryResolver =
        Query
          { user,
            testUnion = Just . TestUnionUser <$> user,
            fail1 = liftEither alwaysFail,
            fail2 = fail "fail with failRes"
          },
      mutationResolver = Mutation {createUser = const user},
      subscriptionResolver =
        Subscription
          { newUser = subscribe [Channel] (pure $ const user),
            newAddress = subscribe [Channel] (pure resolveAddress)
          }
    }
  where
    user :: Applicative m => m (User m)
    user =
      pure
        User
          { name = pure "testName",
            email = pure "",
            address = resolveAddress,
            office = resolveAddress,
            friend = pure Nothing
          }
    -----------------------------------------------------
    resolveAddress :: Applicative m => a -> m (Address m)
    resolveAddress _ =
      pure
        Address
          { city = pure "",
            houseNumber = pure 0,
            street = const $ pure Nothing
          }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
