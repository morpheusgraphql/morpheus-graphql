{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Deprecated.API
  ( gqlRoot
  , Actions
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
import           Data.Morpheus.Kind             ( ENUM
                                                , INPUT_OBJECT
                                                , KIND
                                                , OBJECT
                                                , SCALAR
                                                , UNION
                                                , INPUT_UNION
                                                )
import           Data.Morpheus.Types            ( EventContent
                                                , GQLRootResolver(..)
                                                , GQLScalar(..)
                                                , GQLType(..)
                                                , ID
                                                , ResM
                                                , Resolver
                                                , ScalarValue(..)
                                                , StreamM
                                                , gqlResolver
                                                , gqlStreamResolver
                                                )

type instance KIND CityID = ENUM

type instance KIND Euro = SCALAR

type instance KIND UniqueID = INPUT_OBJECT

type instance KIND Coordinates = INPUT_OBJECT

type instance KIND Address = OBJECT

type instance KIND (User res) = OBJECT

type instance KIND (MyUnion res) = UNION

type instance KIND Cat = INPUT_OBJECT

type instance KIND Dog = INPUT_OBJECT

type instance KIND Bird = INPUT_OBJECT

type instance KIND Animal = INPUT_UNION

newtype Cat = Cat
  { catName :: Text
  } deriving (Show, Generic, GQLType)

newtype Dog = Dog
  { dogName :: Text
  } deriving (Show, Generic, GQLType)

newtype Bird = Bird
  { birdName :: Text
  } deriving (Show, Generic, GQLType)

data Animal
  = CAT Cat
  | DOG Dog
  | BIRD Bird
  deriving (Show, Generic, GQLType)

newtype UniqueID = UniqueID
  { uid :: Text
  } deriving (Show, Generic, GQLType)

data MyUnion res
  = USER (User res)
  | ADDRESS Address
  deriving (Generic, GQLType)

data CityID
  = Paris
  | BLN
  | HH
  deriving (Generic, GQLType)

data Euro =
  Euro Int
       Int
  deriving (Generic, GQLType)

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

data Coordinates = Coordinates
  { latitude  :: Euro
  , longitude :: [Maybe [[UniqueID]]]
  } deriving (Generic)

instance GQLType Coordinates where
  description _ = "just random latitude and longitude"

data Address = Address
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  } deriving (Generic, GQLType)

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
  description _ = "Custom Description for Client Defined User Type"

type instance KIND (A a) = OBJECT

newtype A a = A
  { wrappedA :: a
  } deriving (Generic, GQLType)

fetchAddress :: Monad m => Euro -> m (Either String Address)
fetchAddress _ = return $ Right $ Address " " "" 0

fetchUser :: Monad m => m (Either String (User (Resolver m)))
fetchUser = return $ Right $ User
  { name    = "George"
  , email   = "George@email.com"
  , address = const resolveAddress
  , office  = resolveOffice
  , home    = HH
  , myUnion = const $ return $ USER unionUser
  }
 where
  unionAddress =
    Address { city = "Hamburg", street = "Street", houseNumber = 20 }
  -- Office
  resolveOffice OfficeArgs { cityID = Paris } =
    gqlResolver $ fetchAddress (Euro 1 1)
  resolveOffice OfficeArgs { cityID = BLN } =
    gqlResolver $ fetchAddress (Euro 1 2)
  resolveOffice OfficeArgs { cityID = HH } =
    gqlResolver $ fetchAddress (Euro 1 3)
  resolveAddress = gqlResolver $ fetchAddress (Euro 1 0)
  unionUser      = User
    { name    = "David"
    , email   = "David@email.com"
    , address = const resolveAddress
    , office  = resolveOffice
    , home    = BLN
    , myUnion = const $ return $ ADDRESS unionAddress
    }

data Actions
  = UPDATE_USER
  | UPDATE_ADDRESS
  deriving (Show, Eq, Ord)

data instance  EventContent Actions = Update{contentID :: Int,
                                             contentMessage :: Text}

newtype AnimalArgs = AnimalArgs
  { animal :: Animal
  } deriving (Show, Generic)

setAnimalResolver :: AnimalArgs -> ResM Text
setAnimalResolver AnimalArgs { animal } = return $ pack $ show animal

data Query = Query
  { user       :: () -> ResM (User ResM)
  , wrappedA1  :: A (Int, Text)
  , setAnimal  :: AnimalArgs -> ResM Text
  , wrappedA2  :: A Text
  , integerSet :: Set Int
  , textIntMap :: Map Text Int
  } deriving (Generic)

data Mutation = Mutation
  { createUser    :: () -> StreamM Actions (User (StreamM Actions))
  , createAddress :: () -> StreamM Actions Address
  } deriving (Generic)

data Subscription = Subscription
  { newAddress :: () -> ([Actions], EventContent Actions -> ResM Address)
  , newUser    :: () -> ([Actions], EventContent Actions -> ResM (User ResM))
  } deriving (Generic)

gqlRoot :: GQLRootResolver IO Actions Query Mutation Subscription
gqlRoot = GQLRootResolver
  { queryResolver        = return Query
    { user       = const $ gqlResolver fetchUser
    , wrappedA1  = A (0, "")
    , setAnimal  = setAnimalResolver
    , wrappedA2  = A ""
    , integerSet = S.fromList [1, 2]
    , textIntMap = M.fromList [("robin", 1), ("carl", 2)]
    }
  , mutationResolver     = return Mutation { createAddress , createUser }
  , subscriptionResolver = return Subscription { newAddress , newUser }
  }
 where
  newUser _ = ([UPDATE_ADDRESS], \Update{} -> gqlResolver fetchUser)
  newAddress _ =
    ( [UPDATE_ADDRESS]
    , \Update { contentID } -> gqlResolver $ fetchAddress (Euro contentID 0)
    )
  createUser _ = gqlStreamResolver
    [ ( [UPDATE_USER]
      , Update { contentID = 12, contentMessage = "some message for user" }
      )
    ]
    fetchUser
  createAddress _ = gqlStreamResolver
    [ ( [UPDATE_ADDRESS]
      , Update { contentID = 10, contentMessage = "message for address" }
      )
    ]
    (fetchAddress (Euro 1 0))
