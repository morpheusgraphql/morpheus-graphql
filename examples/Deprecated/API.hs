{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE TypeOperators     #-}

module Deprecated.API
  ( gqlRoot
  , Channel
  , Content
  ) where

import           Data.Map            (Map)
import qualified Data.Map            as M (fromList)
import           Data.Set            (Set)
import qualified Data.Set            as S (fromList)
import           Data.Text           (Text, pack)
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)

-- MORPHEUS
import           Data.Morpheus.Kind  (ENUM, INPUT_OBJECT, INPUT_UNION, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types (Event (..), GQLRootResolver (..), GQLScalar (..), GQLType (..), ID, IOMutRes,
                                      IORes, IOSubRes, Resolver, ScalarValue (..), mutResolver, resolver)

newtype Cat = Cat
  { catName :: Text
  } deriving (Show, Generic)

instance GQLType Cat where
  type KIND Cat = INPUT_OBJECT

newtype Dog = Dog
  { dogName :: Text
  } deriving (Show, Generic)

instance GQLType Dog where
  type KIND Dog = INPUT_OBJECT

newtype Bird = Bird
  { birdName :: Text
  } deriving (Show, Generic)

instance GQLType Bird where
  type KIND Bird = INPUT_OBJECT

data Animal
  = CAT Cat
  | DOG Dog
  | BIRD Bird
  deriving (Show, Generic)

instance GQLType Animal where
  type KIND Animal = INPUT_UNION

newtype UniqueID = UniqueID
  { uid :: Text
  } deriving (Show, Generic)

instance GQLType UniqueID where
  type KIND UniqueID = INPUT_OBJECT

data MyUnion res
  = USER (User res)
  | ADDRESS Address
  deriving (Generic)

instance Typeable a => GQLType (MyUnion a) where
  type KIND (MyUnion a) = UNION

data CityID
  = Paris
  | BLN
  | HH
  deriving (Generic)

instance GQLType CityID where
  type KIND CityID = ENUM

data Euro =
  Euro Int
       Int
  deriving (Generic)

instance GQLType Euro where
  type KIND Euro = SCALAR

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

data Coordinates = Coordinates
  { latitude  :: Euro
  , longitude :: [Maybe [[UniqueID]]]
  } deriving (Generic)

instance GQLType Coordinates where
  type KIND Coordinates = INPUT_OBJECT
  description _ = "just random latitude and longitude"

data Address = Address
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  } deriving (Generic)

instance GQLType Address where
  type KIND Address = OBJECT

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
  , address :: AddressArgs -> m Address
  , office  :: OfficeArgs -> m Address
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

fetchAddress :: Monad m => Euro -> m (Either String Address)
fetchAddress _ = return $ Right $ Address " " "" 0

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
    unionAddress = Address {city = "Hamburg", street = "Street", houseNumber = 20}
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
  } deriving (Show, Generic)

setAnimalResolver :: AnimalArgs -> IORes Text
setAnimalResolver AnimalArgs {animal} = return $ pack $ show animal

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

type SubRes a = IOSubRes Channel Content a

data Mutation = Mutation
  { createUser    :: () -> MutRes (User MutRes)
  , createAddress :: () -> MutRes Address
  } deriving (Generic)

data Subscription = Subscription
  { newAddress :: () -> SubRes Address
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
    newUser _ = Event [UPDATE_USER] $ \Update {} -> resolver fetchUser
    newAddress _ = Event [UPDATE_ADDRESS] $ \Update {contentID} -> resolver $ fetchAddress (Euro contentID 0)
    createUser _ =
      mutResolver [Event [UPDATE_USER] (Update {contentID = 12, contentMessage = "some message for user"})] fetchUser
    createAddress :: () -> MutRes Address
    createAddress _ =
      mutResolver
        [Event [UPDATE_ADDRESS] (Update {contentID = 10, contentMessage = "message for address"})]
        (fetchAddress (Euro 1 0))
