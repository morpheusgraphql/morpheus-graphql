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
import           Control.Monad.Trans            ( lift )
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
                                                , IORes
                                                , IOMutRes
                                                , liftM
                                                , liftEitherM
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

type DB_ID = Int

data Content = Update
  { contentID      :: DB_ID
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
    { queryUser     = const $ liftEitherM getDBUser
    , queryAnimal   = \QueryAnimalArgs { queryAnimalArgsAnimal } ->
                        pure (pack $ show queryAnimalArgsAnimal)
    , querySet      = constRes $ S.fromList [1, 2]
    , querySomeMap  = constRes $ M.fromList [("robin", 1), ("carl", 2)]
    , queryWrapped1 = constRes $ A (0, "some value")
    , queryWrapped2 = constRes $ A ""
    }
  -------------------------------------------------------------
  mutationResolver = Mutation { mutationCreateUser, mutationCreateAddress }
   where
    mutationCreateUser _ =
      MutResolver { mutEvents = [userUpdate], mutResolver = lift setDBUser }
    -------------------------
    mutationCreateAddress _ = MutResolver { mutEvents   = [addressUpdate]
                                          , mutResolver = lift setDBAddress
                                          }
  ----------------------------------------------------------------
  subscriptionResolver = Subscription { subscriptionNewAddress
                                      , subscriptionNewUser
                                      }
   where
    subscriptionNewUser _ = SubResolver [UPDATE_USER] subResolver
      where subResolver (Event _ Update{}) = resolveUser
    subscriptionNewAddress _ = SubResolver [UPDATE_ADDRESS] subResolver
     where
      subResolver (Event _ Update { contentID }) =
        liftM (getDBAddress contentID)


-- Resolvers ----------------------------------------------------------------
resolveUser :: Resolver QUERY IO APIEvent (User (Resolver QUERY IO APIEvent))
resolveUser = liftEitherM getDBUser

-- Events ----------------------------------------------------------------
addressUpdate :: APIEvent
addressUpdate = Event
  [UPDATE_ADDRESS]
  (Update { contentID = 10, contentMessage = "message for address" })

userUpdate :: APIEvent
userUpdate = Event
  [UPDATE_USER]
  (Update { contentID = 12, contentMessage = "some message for user" })

-- DB::Getter --------------------------------------------------------------------
getDBAddress :: DB_ID -> IO (Address (IORes APIEvent))
getDBAddress _unusedID = pure Address { addressCity        = constRes ""
                                      , addressStreet      = constRes ""
                                      , addressHouseNumber = constRes 0
                                      }

getDBUser :: IO (Either String (User (Resolver QUERY IO APIEvent)))
getDBUser = pure $ Right User { userName    = constRes "George"
                              , userEmail   = constRes "George@email.com"
                              , userAddress = const $ liftM (getDBAddress 12)
                              , userOffice  = constRes Nothing
                              , userHome    = constRes HH
                              , userEntity  = constRes Nothing
                              }


-- DB::Setter --------------------------------------------------------------------
setDBAddress :: IO (Address (IOMutRes APIEvent))
setDBAddress = pure Address { addressCity        = constRes ""
                            , addressStreet      = constRes ""
                            , addressHouseNumber = constRes 0
                            }

setDBUser :: IO (User (IOMutRes APIEvent))
setDBUser = pure User { userName    = constRes "George"
                      , userEmail   = constRes "George@email.com"
                      , userAddress = const $ liftM setDBAddress
                      , userOffice  = constRes Nothing
                      , userHome    = constRes HH
                      , userEntity  = constRes Nothing
                      }
