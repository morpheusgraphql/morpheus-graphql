{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}


module Server.Sophisticated.API
  ( api
  , EVENT
  , gqlRoot
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
import           GHC.Generics                   ( Generic )

-- MORPHEUS
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocumentWithNamespace
                                                )
import           Data.Morpheus.Kind             ( INPUT
                                                , SCALAR
                                                )
import           Data.Morpheus.Types            ( Event(..)
                                                , GQLRootResolver(..)
                                                , GQLScalar(..)
                                                , GQLType(..)
                                                , ID
                                                , Resolver
                                                , ScalarValue(..)
                                                , constRes
                                                , IORes
                                                , IOMutRes
                                                , liftEither
                                                , ResolveQ
                                                , ResolveM
                                                , ResolveS
                                                , failRes
                                                , publish
                                                , subscribe
                                                , WithOperation
                                                , Input
                                                , Stream
                                                )

newtype A a = A { wrappedA :: a } 
  deriving (Generic, GQLType)

type AIntText = A (Int, Text)

type AText = A Text

type SetInt = Set Int

type MapTextInt = Map Text Int


$(importGQLDocumentWithNamespace "src/Server/Sophisticated/shared.gql")


$(importGQLDocumentWithNamespace "src/Server/Sophisticated/api.gql")


data Animal
  = AnimalCat Cat
  | AnimalDog Dog
  | AnimalBird Bird
  | Giraffe  { giraffeName :: Text }
  | UnidentifiedSpecie
  deriving (Show, Generic)

instance GQLType Animal where
  type KIND Animal = INPUT

data Euro =
  Euro Int
       Int
  deriving (Show, Generic)

instance GQLType Euro where
  type KIND Euro = SCALAR

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

data Channel = USER | ADDRESS
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int  }

type EVENT = (Event Channel Content)

api :: Input api -> Stream api EVENT IO
api = interpreter gqlRoot

gqlRoot :: GQLRootResolver IO EVENT Query Mutation Subscription
gqlRoot = GQLRootResolver { queryResolver
                          , mutationResolver
                          , subscriptionResolver
                          }
 where
  queryResolver = Query
    { queryUser     = resolveUser
    , queryAnimal   = resolveAnimal
    , querySet      = pure $ S.fromList [1, 2]
    , querySomeMap  = pure $ M.fromList [("robin", 1), ("carl", 2)]
    , queryWrapped1 = constRes $ A (0, "some value")
    , queryWrapped2 = pure $ A ""
    , queryFail1    = failRes "fail example"
    , queryFail2    = liftEither alwaysFail
    , queryShared = pure SharedType { sharedTypeName = pure "some name" }
    }
  -------------------------------------------------------------
  mutationResolver = Mutation { mutationCreateUser    = resolveCreateUser
                              , mutationCreateAddress = resolveCreateAdress
                              , mutationSetAdress     = resolveSetAdress
                              }
  subscriptionResolver = Subscription
    { subscriptionNewUser    = resolveNewUser
    , subscriptionNewAddress = resolveNewAdress
    }


-- Resolve QUERY

alwaysFail :: IO (Either String a)
alwaysFail = pure $ Left "fail example"

resolveUser :: ResolveQ EVENT IO User
resolveUser = liftEither (getDBUser (Content 2))

resolveAnimal :: QueryAnimalArgs -> IORes EVENT Text
resolveAnimal QueryAnimalArgs { queryAnimalArgsAnimal } =
  pure (pack $ show queryAnimalArgsAnimal)

-- Resolve MUTATIONS
-- 
-- Mutation With Event Triggering : sends events to subscription  
resolveCreateUser :: ResolveM EVENT IO User
resolveCreateUser = do
  requireAuthorized
  publish [userUpdate]
  liftEither setDBUser

-- Mutation With Event Triggering : sends events to subscription  
resolveCreateAdress :: ResolveM EVENT IO Address
resolveCreateAdress = do
  requireAuthorized
  publish [addressUpdate]
  lift setDBAddress

-- Mutation Without Event Triggering  
resolveSetAdress :: ResolveM EVENT IO Address
resolveSetAdress = lift setDBAddress


-- Resolve SUBSCRIPTION
resolveNewUser :: ResolveS EVENT IO User
resolveNewUser = subscribe [USER] $ do 
    requireAuthorized 
    pure subResolver 
  where subResolver (Event _ content) = liftEither (getDBUser content)

resolveNewAdress :: ResolveS EVENT IO Address
resolveNewAdress = subscribe [ADDRESS] $ do
    requireAuthorized
    pure subResolver
  where subResolver (Event _ content) = lift (getDBAddress content)

-- Events ----------------------------------------------------------------
addressUpdate :: EVENT
addressUpdate = Event [ADDRESS] (Content { contentID = 10 })

userUpdate :: EVENT
userUpdate = Event [USER] (Content { contentID = 12 })

-- DB::Getter --------------------------------------------------------------------
getDBAddress :: Content -> IO (Address (IORes EVENT))
getDBAddress _id = do
  city   <- dbText
  street <- dbText
  number <- dbInt
  pure Address { addressCity        = pure city
               , addressStreet      = pure street
               , addressHouseNumber = pure number
               }

getDBUser :: Content -> IO (Either String (User (IORes EVENT)))
getDBUser _ = do
  Person { name, email } <- dbPerson
  pure $ Right User { userName    = pure name
                    , userEmail   = pure email
                    , userAddress = const $ lift (getDBAddress (Content 12))
                    , userOffice  = constRes Nothing
                    , userHome    = pure HH
                    , userEntity  = pure [
                          MyUnionAddress Address{
                            addressCity        = pure "city"
                            , addressStreet      = pure "street"
                            , addressHouseNumber = pure 1
                          },
                          MyUnionUser User {
                              userName    = pure name
                            , userEmail   = pure email
                            , userAddress = const $ lift (getDBAddress (Content 12))
                            , userOffice  = constRes Nothing
                            , userHome    = pure HH
                            , userEntity = pure []
                          }
                        ]
                    }

-- DB::Setter --------------------------------------------------------------------
setDBAddress :: IO (Address (IOMutRes EVENT))
setDBAddress = do
  city        <- dbText
  street      <- dbText
  houseNumber <- dbInt
  pure Address { addressCity        = pure city
               , addressStreet      = pure street
               , addressHouseNumber = pure houseNumber
               }

setDBUser :: IO (Either String (User (IOMutRes EVENT)))
setDBUser = do
  Person { name, email } <- dbPerson
  pure $ Right $ 
       User { userName    = pure name
            , userEmail   = pure email
            , userAddress = const $ lift setDBAddress
            , userOffice  = constRes Nothing
            , userHome    = pure HH
            , userEntity  = pure []
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


requireAuthorized :: WithOperation o => Resolver o e IO ()
requireAuthorized = pure ()