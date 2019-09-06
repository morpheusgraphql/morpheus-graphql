{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE TypeOperators     #-}

module Sophisticated.API
  ( gqlRoot
  , Channel
  , Content
  ) where

import           Data.Map               (Map)
import qualified Data.Map               as M (fromList)
import           Data.Set               (Set)
import qualified Data.Set               as S (fromList)
import           Data.Text              (Text, pack)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)

import           Data.Morpheus.Document (importGQLDocument)

-- MORPHEUS
import           Data.Morpheus.Kind     (INPUT_UNION, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types    (Event (..), GQLRootResolver (..), GQLScalar (..), GQLType (..), ID, IOMutRes,
                                         IORes, IOSubRes, Resolver, ScalarValue (..), SubResolver (..), constRes,
                                         mutResolver, resolver)

importGQLDocument "examples/Sophisticated/api.gql"

data Animal
  = CAT Cat
  | DOG Dog
  | BIRD Bird
  deriving (Generic)

instance GQLType Animal where
  type KIND Animal = INPUT_UNION

data MyUnion m
  = USER (User m)
  | ADDRESS (Address m)
  deriving (Generic)

instance Typeable a => GQLType (MyUnion a) where
  type KIND (MyUnion a) = UNION

data Euro =
  Euro Int
       Int
  deriving (Generic)

instance GQLType Euro where
  type KIND Euro = SCALAR

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

data AddressArgs = AddressArgs
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic)

data OfficeArgs = OfficeArgs
  { zipCode :: Maybe [[Maybe [ID]]]
  , cityID  :: CityID
  } deriving (Generic)

data User m = User
  { name    :: Text
  , email   :: Text
  , address :: AddressArgs -> m (Address m)
  , office  :: OfficeArgs -> m (Address m)
  , myUnion :: () -> m (MyUnion m)
  , home    :: CityID
  } deriving (Generic)

instance Typeable a => GQLType (User a) where
  type KIND (User a) = OBJECT
  description _ = "Custom Description for Client Defined User Type"

instance Typeable a => GQLType (A a) where
  type KIND (A a) = OBJECT

newtype A a = A
  { wrappedA :: a
  } deriving (Generic)

fetchAddress :: Monad m => Euro -> m (Either String (Address (Resolver m)))
fetchAddress _ = return $ Right Address {city = constRes "", street = constRes "", houseNumber = constRes 0}

fetchUser :: Monad m => m (Either String (User (Resolver m)))
fetchUser =
  return $
  Right $
  User
    { name = "George"
    , email = "George@email.com"
    , address = const resolveAddress
    , office = resolveOffice
    , home = HH
    , myUnion = const $ return $ USER unionUser
    }
  where
    unionAddress = Address {city = constRes "Hamburg", street = constRes "Street", houseNumber = constRes 20}
  -- Office
    resolveOffice OfficeArgs {cityID = Paris} = resolver $ fetchAddress (Euro 1 1)
    resolveOffice OfficeArgs {cityID = BLN}   = resolver $ fetchAddress (Euro 1 2)
    resolveOffice OfficeArgs {cityID = HH}    = resolver $ fetchAddress (Euro 1 3)
    resolveAddress = resolver $ fetchAddress (Euro 1 0)
    unionUser =
      User
        { name = "David"
        , email = "David@email.com"
        , address = const resolveAddress
        , office = resolveOffice
        , home = BLN
        , myUnion = const $ return $ ADDRESS unionAddress
        }

newtype AnimalArgs = AnimalArgs
  { animal :: Animal
  } deriving (Generic)

setAnimalResolver :: AnimalArgs -> IORes Text
setAnimalResolver AnimalArgs {animal} = return "TODO: $ pack $ show animal"

data Query = Query
  { user       :: () -> IORes (User IORes)
  , wrappedA1  :: A (Int, Text)
  , setAnimal  :: AnimalArgs -> IORes Text
  , wrappedA2  :: A Text
  , integerSet :: Set Int
  , textIntMap :: Map Text Int
  } deriving (Generic)

data Channel
  = UPDATE_USER
  | UPDATE_ADDRESS
  deriving (Show, Eq, Ord)

data Content = Update
  { contentID      :: Int
  , contentMessage :: Text
  }

type MutRes = IOMutRes Channel Content

type SubRes = IOSubRes Channel Content

data Mutation = Mutation
  { createUser    :: () -> MutRes (User MutRes)
  , createAddress :: () -> MutRes (Address MutRes)
  } deriving (Generic)

data Subscription = Subscription
  { newAddress :: () -> SubRes (Address IORes)
  , newUser    :: () -> SubRes (User IORes)
  } deriving (Generic)

gqlRoot :: GQLRootResolver IO Channel Content Query Mutation Subscription
gqlRoot =
  GQLRootResolver
    { queryResolver =
        return
          Query
            { user = const $ resolver fetchUser
            , wrappedA1 = A (0, "")
            , setAnimal = setAnimalResolver
            , wrappedA2 = A ""
            , integerSet = S.fromList [1, 2]
            , textIntMap = M.fromList [("robin", 1), ("carl", 2)]
            }
    , mutationResolver = return Mutation {createAddress, createUser}
    , subscriptionResolver = return Subscription {newAddress, newUser}
    }
  where
    newUser _ = SubResolver {subChannels = [UPDATE_USER], subResolver = \(Event _ Update {}) -> resolver fetchUser}
    newAddress _ =
      SubResolver [UPDATE_ADDRESS] $ \(Event _ Update {contentID}) -> resolver $ fetchAddress (Euro contentID 0)
    createUser _ =
      mutResolver [Event [UPDATE_USER] (Update {contentID = 12, contentMessage = "some message for user"})] fetchUser
    createAddress :: () -> MutRes (Address MutRes)
    createAddress _ =
      mutResolver
        [Event [UPDATE_ADDRESS] (Update {contentID = 10, contentMessage = "message for address"})]
        (fetchAddress (Euro 1 0))
