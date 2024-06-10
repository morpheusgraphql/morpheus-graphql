{-# LANGUAGE DataKinds #-}
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
import Data.Morpheus.Subscriptions (Event)
import Data.Morpheus.Types
  ( Arg (..),
    DecodeScalar (..),
    Deprecated (..),
    Describe (..),
    DropNamespace (..),
    EncodeScalar (..),
    GQLRequest,
    GQLResponse,
    GQLType (..),
    ID (..),
    Rename (..),
    RootResolver (..),
    ScalarValue (..),
    TypeGuard (..),
    Undefined,
    defaultRootResolver,
    liftEither,
    subscribe,
  )
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude
  ( Applicative (..),
    Either (..),
    Eq (..),
    IO,
    Int,
    Maybe (..),
    Show (..),
    String,
    const,
    ($),
    (*),
    (+),
  )

data TestScalar
  = TestScalar
      Int
      Int
  deriving (Show, Generic)

instance GQLType TestScalar where
  type KIND TestScalar = SCALAR

instance DecodeScalar TestScalar where
  decodeScalar _ = pure (TestScalar 1 0)

instance EncodeScalar TestScalar where
  encodeScalar (TestScalar x y) = Int (x * 100 + y)

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
            queryTestUnion =
              pure $ Just (TestUnionUser queryUser),
            queryPerson =
              pure
                ( ResolveType
                    User
                      { userName = pure "test Person Name",
                        userEmail = pure "",
                        userAddress = resolveAddress,
                        userOffice = resolveAddress,
                        userFriend = pure Nothing
                      }
                ),
            queryTestEnum = \(Arg enum) ->
              pure
                [ enum,
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
    queryUser :: (Applicative m) => m (User m)
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
    resolveAddress :: (Applicative m) => a -> m (Address m)
    resolveAddress _ =
      pure
        Address
          { addressCity = pure "",
            addressHouseNumber = pure 0,
            addressStreet = const $ pure Nothing
          }

rootExt :: RootResolver IO EVENT ExtQuery Undefined Undefined
rootExt =
  defaultRootResolver
    { queryResolver =
        ExtQuery
          { fail1 = liftEither alwaysFail,
            fail2 = fail "fail with MonadFail",
            type' = \(Arg TypeInput {data'}) -> pure data'
          }
    }

api :: GQLRequest -> IO GQLResponse
api =
  runApp
    ( deriveApp root
        <> deriveApp rootExt
    )
