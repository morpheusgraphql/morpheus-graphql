{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

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
import           Data.Morpheus.Kind     (INPUT_UNION, OBJECT, SCALAR)
import           Data.Morpheus.Types    (Event (..), GQLRootResolver (..), GQLScalar (..), GQLType (..), ID, IOMutRes,
                                         IORes, IOSubRes, Resolver, ScalarValue (..), SubResolver (..), constRes,
                                         mutResolver, resolver)

importGQLDocument "examples/Sophisticated/api.gql"

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

type MutRes = IOMutRes Channel Content

type SubRes = IOSubRes Channel Content

gqlRoot :: GQLRootResolver IO Channel Content (Query IORes) (Mutation MutRes) (Subscription SubRes IORes)
gqlRoot = GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver}
  where
    queryResolver =
      return
        Query
          { user = const $ resolver fetchUser
          , wrappedA1 = constRes $ A (0, "")
          , setAnimal = \SetAnimalArgs {animal} -> return $ pack $ show animal
          , wrappedA2 = constRes $ A ""
          , integerSet = constRes $ S.fromList [1, 2]
          , textIntMap = constRes $ M.fromList [("robin", 1), ("carl", 2)]
          }
    -------------------------------------------------------------
    mutationResolver = return Mutation {createAddress, createUser}
      where
        createUser _ =
          mutResolver
            [Event [UPDATE_USER] (Update {contentID = 12, contentMessage = "some message for user"})]
            fetchUser
        createAddress :: () -> MutRes (Address MutRes)
        createAddress _ =
          mutResolver
            [Event [UPDATE_ADDRESS] (Update {contentID = 10, contentMessage = "message for address"})]
            (fetchAddress (Euro 1 0))
    ----------------------------------------------------------------
    subscriptionResolver = return Subscription {newAddress, newUser}
      where
        newUser () = SubResolver {subChannels = [UPDATE_USER], subResolver}
          where
            subResolver (Event _ Update {}) = resolver fetchUser
        newAddress () = SubResolver {subChannels = [UPDATE_ADDRESS], subResolver}
          where
            subResolver (Event _ Update {contentID}) = resolver $ fetchAddress (Euro contentID 0)

fetchAddress :: Monad m => Euro -> m (Either String (Address (Resolver m)))
fetchAddress _ = return $ Right Address {city = constRes "", street = constRes "", houseNumber = constRes 0}

fetchUser :: Monad m => m (Either String (User (Resolver m)))
fetchUser =
  return $
  Right $
  User
    { name = constRes "George"
    , email = constRes "George@email.com"
    , address = const resolveAddress
    , office = resolveOffice
    , home = constRes HH
    , myUnion = constRes $ MyUnionUser unionUser
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
        { name = constRes "David"
        , email = constRes "David@email.com"
        , address = const resolveAddress
        , office = resolveOffice
        , home = constRes BLN
        , myUnion = const $ return $ MyUnionAddress unionAddress
        }
