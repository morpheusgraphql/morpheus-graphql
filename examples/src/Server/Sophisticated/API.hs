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


module Server.Sophisticated.API
  ( gqlRoot
  , APIEvent
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
                                                ( fromList )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
                                                ( fromList )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )

-- MORPHEUS
import           Data.Morpheus.Document         ( importGQLDocumentWithNamespace
                                                )
import           Data.Morpheus.Kind             ( INPUT_UNION
                                                , OBJECT
                                                , SCALAR
                                                )
import           Data.Morpheus.Types            ( Event(..)
                                                , GQLRootResolver(..)
                                                , GQLScalar(..)
                                                , GQLType(..)
                                                , ID
                                                , QUERY
                                                , Resolver(..)
                                                , ScalarValue(..)
                                                , constRes
                                                )


$(importGQLDocumentWithNamespace "src/Server/Sophisticated/api.gql")

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


gqlRoot :: GQLRootResolver IO APIEvent Query Mutation Subscription
gqlRoot = GQLRootResolver { queryResolver
                          , mutationResolver
                          , subscriptionResolver
                          }
 where
  queryResolver = Query
    { queryUser     = const fetchUser
    , queryAnimal   = \QueryAnimalArgs { queryAnimalArgsAnimal } ->
                        pure (pack $ show queryAnimalArgsAnimal)
    , querySet      = constRes $ S.fromList [1, 2]
    , queryMap      = constRes $ M.fromList [("robin", 1), ("carl", 2)]
    , queryWrapped1 = constRes $ A (0, "some value")
    , queryWrapped2 = constRes $ A ""
    }
  -------------------------------------------------------------
  mutationResolver = Mutation { mutationCreateUser, mutationCreateAddress }
   where
    mutationCreateUser _ = MutResolver
      [ Event
          [UPDATE_USER]
          (Update { contentID = 12, contentMessage = "some message for user" })
      ]
      (pure User { userName    = constRes "George"
                 , userEmail   = constRes "George@email.com"
                 , userAddress = constRes mutationAddress
                 , userOffice  = constRes Nothing
                 , userHome    = constRes HH
                 , userEntity  = constRes Nothing
                 }
      )
    -------------------------
    mutationCreateAddress _ = MutResolver
      [ Event
          [UPDATE_ADDRESS]
          (Update { contentID = 10, contentMessage = "message for address" })
      ]
      (pure mutationAddress)
  ----------------------------------------------------------------
  subscriptionResolver = Subscription { subscriptionNewAddress
                                      , subscriptionNewUser
                                      }
   where
    subscriptionNewUser () = SubResolver [UPDATE_USER] subResolver
      where subResolver (Event _ Update{}) = fetchUser
    subscriptionNewAddress () = SubResolver [UPDATE_ADDRESS] subResolver
      where subResolver (Event _ Update { contentID }) = fetchAddress
  ----------------------------------------------------------------------------------------------
  fetchAddress = pure Address { addressCity        = constRes ""
                              , addressStreet      = constRes ""
                              , addressHouseNumber = constRes 0
                              }
  fetchUser
    :: Resolver
         QUERY
         IO
         (Event Channel Content)
         (User (Resolver QUERY IO (Event Channel Content)))
  fetchUser = pure User { userName    = constRes "George"
                        , userEmail   = constRes "George@email.com"
                        , userAddress = const fetchAddress
                        , userOffice  = constRes Nothing
                        , userHome    = constRes HH
                        , userEntity  = constRes Nothing
                        }
  mutationAddress = Address { addressCity        = constRes ""
                            , addressStreet      = constRes ""
                            , addressHouseNumber = constRes 0
                            }
