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

module Feature.Holistic.API
  ( api,
  )
where

import Control.Monad.Fail (fail)
import Data.Morpheus (deriveApp, runApp)
import Data.Morpheus.Document
  ( importGQLDocument,
    importGQLDocumentWithNamespace,
  )
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
  ( Event,
    GQLRequest,
    GQLResponse,
    GQLScalar (..),
    GQLType (..),
    ID (..),
    RootResolver (..),
    ScalarValue (..),
    Undefined (..),
    liftEither,
    subscribe,
  )
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude
  ( ($),
    (*),
    (+),
    (.),
    (<$>),
    Applicative (..),
    Either (..),
    Eq (..),
    IO,
    Int,
    Maybe (..),
    Show (..),
    String,
    const,
    id,
  )

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

importGQLDocumentWithNamespace "test/Feature/Holistic/schema.gql"

importGQLDocument "test/Feature/Holistic/schema-ext.gql"

alwaysFail :: IO (Either String a)
alwaysFail = pure $ Left "fail with Either"

root :: RootResolver IO EVENT Query Mutation Subscription
root =
  RootResolver
    { queryResolver =
        Query
          { queryUser,
            queryTestUnion = Just . TestUnionUser <$> queryUser,
            queryPerson =
              pure
                Person
                  { personName = pure (Just "test Person Name")
                  },
            queryTestEnum =
              \QueryTestEnumArgs
                 { queryTestEnumArgsEnum
                 } ->
                  pure
                    [ queryTestEnumArgsEnum,
                      CollidingEnumEnumA
                    ]
          },
      mutationResolver =
        Mutation
          { mutationCreateUser = const queryUser
          },
      subscriptionResolver =
        Subscription
          { subscriptionNewUser = subscribe Channel (pure $ const queryUser),
            subscriptionNewAddress = subscribe Channel (pure resolveAddress)
          }
    }
  where
    queryUser :: Applicative m => m (User m)
    queryUser =
      pure
        User
          { userName = pure "testName",
            userEmail = pure "",
            userAddress = resolveAddress,
            userOffice = resolveAddress,
            userFriend = pure Nothing
          }
    -----------------------------------------------------
    resolveAddress :: Applicative m => a -> m (Address m)
    resolveAddress _ =
      pure
        Address
          { addressCity = pure "",
            addressHouseNumber = pure 0,
            addressStreet = const $ pure Nothing
          }

rootExt :: RootResolver IO EVENT ExtQuery Undefined Undefined
rootExt =
  RootResolver
    { queryResolver =
        ExtQuery
          { fail1 = liftEither alwaysFail,
            fail2 = fail "fail with MonadFail",
            type' = \TypeArgs {in' = TypeInput {data'}} -> pure data'
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api =
  runApp
    ( deriveApp root
        <> deriveApp rootExt
    )
