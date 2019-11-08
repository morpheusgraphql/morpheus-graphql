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
                                                , Resolver(..)
                                                , ScalarValue(..)
                                                , constRes
                                                , IORes
                                                , IOMutRes
                                                , liftEither
                                                , IOSubRes
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


data Channel = USER | ADDRESS
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int  }

type APIEvent = (Event Channel Content)

gqlRoot :: GQLRootResolver IO APIEvent Query Mutation Subscription
gqlRoot = GQLRootResolver { queryResolver
                          , mutationResolver
                          , subscriptionResolver
                          }
 where
  queryResolver = Query
    { queryUser     = resolveUser
    , queryAnimal   = resolveAnimal
    , querySet      = constRes $ S.fromList [1, 2]
    , querySomeMap  = constRes $ M.fromList [("robin", 1), ("carl", 2)]
    , queryWrapped1 = constRes $ A (0, "some value")
    , queryWrapped2 = constRes $ A ""
    }
  -------------------------------------------------------------
  mutationResolver = Mutation { mutationCreateUser    = resolveCreateUser
                              , mutationCreateAddress = resolveCreateAdress
                              }
  subscriptionResolver = Subscription
    { subscriptionNewUser    = resolveNewUser
    , subscriptionNewAddress = resolveNewAdress
    }

-- Resolve QUERY
resolveUser :: () -> IORes APIEvent (User (IORes APIEvent))
resolveUser _args = liftEither (getDBUser (Content 2))

resolveAnimal :: QueryAnimalArgs -> IORes APIEvent Text
resolveAnimal QueryAnimalArgs { queryAnimalArgsAnimal } =
  pure (pack $ show queryAnimalArgsAnimal)

-- Resolve MUTATION 
resolveCreateUser :: () -> IOMutRes APIEvent (User (IOMutRes APIEvent))
resolveCreateUser _args =
  MutResolver { mutEvents = [userUpdate], mutResolver = lift setDBUser }

resolveCreateAdress :: () -> IOMutRes APIEvent (Address (IOMutRes APIEvent))
resolveCreateAdress _args =
  MutResolver { mutEvents = [addressUpdate], mutResolver = lift setDBAddress }

-- Resolve SUBSCRIPTION
resolveNewUser :: () -> IOSubRes APIEvent (User (IORes APIEvent))
resolveNewUser _args = SubResolver { subChannels = [USER], subResolver }
  where subResolver (Event _ content) = liftEither (getDBUser content)

resolveNewAdress :: () -> IOSubRes APIEvent (Address (IORes APIEvent))
resolveNewAdress _args = SubResolver { subChannels = [ADDRESS], subResolver }
  where subResolver (Event _ content) = lift (getDBAddress content)

-- Events ----------------------------------------------------------------
addressUpdate :: APIEvent
addressUpdate = Event [ADDRESS] (Content { contentID = 10 })

userUpdate :: APIEvent
userUpdate = Event [USER] (Content { contentID = 12 })

-- DB::Getter --------------------------------------------------------------------
getDBAddress :: Content -> IO (Address (IORes APIEvent))
getDBAddress _id = do
  city   <- dbText
  street <- dbText
  number <- dbInt
  pure Address { addressCity        = constRes city
               , addressStreet      = constRes street
               , addressHouseNumber = constRes number
               }

getDBUser :: Content -> IO (Either String (User (IORes APIEvent)))
getDBUser _ = do
  Person { name, email } <- dbPerson
  pure $ Right User { userName    = constRes name
                    , userEmail   = constRes email
                    , userAddress = const $ lift (getDBAddress (Content 12))
                    , userOffice  = constRes Nothing
                    , userHome    = constRes HH
                    , userEntity  = constRes Nothing
                    }

-- DB::Setter --------------------------------------------------------------------
setDBAddress :: IO (Address (IOMutRes APIEvent))
setDBAddress = do
  city        <- dbText
  street      <- dbText
  houseNumber <- dbInt
  pure Address { addressCity        = constRes city
               , addressStreet      = constRes street
               , addressHouseNumber = constRes houseNumber
               }

setDBUser :: IO (User (IOMutRes APIEvent))
setDBUser = do
  Person { name, email } <- dbPerson
  pure User { userName    = constRes name
            , userEmail   = constRes email
            , userAddress = const $ lift setDBAddress
            , userOffice  = constRes Nothing
            , userHome    = constRes HH
            , userEntity  = constRes Nothing
            }

-- DB ----------------------
data Person = Person {
  name :: Text,
  email :: Text
}

dbText :: IO Text
dbText = pure "Updated Text"

dbInt :: IO Int
dbInt = pure 11

dbPerson :: IO Person
dbPerson = pure Person { name = "George", email = "George@email.com" }
