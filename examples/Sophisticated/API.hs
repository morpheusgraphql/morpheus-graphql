{-# LANGUAGE DataKinds             #-}
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

module Sophisticated.API
  ( gqlRoot
  , APIEvent
  ) where

import           Data.Map               (Map)
import qualified Data.Map               as M (fromList)
import           Data.Set               (Set)
import qualified Data.Set               as S (fromList)
import           Data.Text              (Text, pack)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)

-- MORPHEUS
import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Kind     (INPUT_UNION, OBJECT, SCALAR)
import           Data.Morpheus.Types    (Event (..), GADTResolver (..), GQLRootResolver (..), GQLScalar (..),
                                         GQLType (..), ID, MutResolver, QUERY, Resolver, ScalarValue (..), resolver)

$(importGQLDocumentWithNamespace "examples/Sophisticated/api.gql")

type AIntText = A (Int, Text)

type AText = A Text

type SetInt = Set Int

type MapTextInt = Map Text Int

data Animal
  = CAT Cat
  | DOG Dog
  | BIRD Bird
  deriving (Show, Generic)

instance GQLType Animal where
  type KIND Animal = INPUT_UNION

data Euro =
  Euro Int
       Int
  deriving (Show, Generic)

instance GQLType Euro where
  type KIND Euro = SCALAR

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

instance Typeable a => GQLType (A a) where
  type KIND (A a) = OBJECT

newtype A a = A
  { wrappedA :: a
  } deriving (Generic)

data Channel
  = UPDATE_USER
  | UPDATE_ADDRESS
  deriving (Show, Eq, Ord)

data Content = Update
  { contentID      :: Int
  , contentMessage :: Text
  }

type APIEvent = (Event Channel Content)

constRes :: Monad m => a -> b -> GADTResolver QUERY m e a
constRes = const . QueryResolver . pure

gqlRoot :: GQLRootResolver IO APIEvent Query Mutation Subscription
gqlRoot = GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver}
  where
    queryResolver =
      return
        Query
          { queryUser = constRes fetchUser
          , queryAnimal = \QueryAnimalArgs {queryAnimalArgsAnimal} -> return (pack $ show queryAnimalArgsAnimal)
          , querySet = constRes $ S.fromList [1, 2]
          , queryMap = constRes $ M.fromList [("robin", 1), ("carl", 2)]
          , queryWrapped1 = constRes $ A (0, "")
          , queryWrapped2 = constRes $ A ""
          }
          where
            ------------------
            fetchUser = User
                        { userName = constRes "George"
                        , userEmail = constRes "George@email.com"
                        , userAddress
                        , userOffice = constRes Nothing
                        , userHome = constRes HH
                        , userEntity = constRes Nothing
                        }
                 where
                    userAddress _ = QueryResolver $ return $  Address {addressCity = constRes "", addressStreet = constRes "", addressHouseNumber = constRes 0}
    -------------------------------------------------------------
    mutationResolver = return Mutation { mutationCreateUser , mutationCreateAddress }
      where
        mutationCreateUser _ =
          MutationResolver
            [Event [UPDATE_USER] (Update {contentID = 12, contentMessage = "some message for user"})]
            (return $ User
                 { userName = constResMut [] "George"
                 , userEmail = constResMut [] "George@email.com"
                 , userAddress = constResMut [] fetchAddressMutation
                 , userOffice = constResMut [] Nothing
                 , userHome = constResMut [] HH
                 , userEntity = constResMut [] Nothing
                 })
        -------------------------
        mutationCreateAddress _ =
          MutationResolver
            [Event [UPDATE_ADDRESS] (Update {contentID = 10, contentMessage = "message for address"})]
            (return fetchAddressMutation)
    ----------------------------------------------------------------
    subscriptionResolver = return Subscription {subscriptionNewAddress, subscriptionNewUser}
      where
        subscriptionNewUser () = SubscriptionResolver [UPDATE_USER] subResolver
          where
            subResolver (Event _ Update {}) = QueryResolver $ pure $  User { userName = constRes "George"
                                            , userEmail = constRes "George@email.com"
                                            , userAddress = const $ QueryResolver $ return $ Address {addressCity = constRes "", addressStreet = constRes "", addressHouseNumber = constRes 0}
                                            , userOffice = constRes Nothing
                                            , userHome = constRes HH
                                            , userEntity = constRes Nothing
                                       }
        subscriptionNewAddress () = SubscriptionResolver [UPDATE_ADDRESS] subResolver
          where
            subResolver (Event _ Update {contentID}) =  QueryResolver $ return $ Address {addressCity = constRes "", addressStreet = constRes "", addressHouseNumber = constRes 0}
    ----------------------------------------------------------------------------------------------
    fetchAddressMutation = Address {
          addressCity = constResMut [] "",
          addressStreet = constResMut []  "",
          addressHouseNumber = constResMut []  0
        }

constResMut :: Monad m =>  [e] -> a -> args -> MutResolver m e a
constResMut list value = const $ MutationResolver list $ return value

