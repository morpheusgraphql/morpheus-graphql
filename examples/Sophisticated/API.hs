{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Sophisticated.API
  ( gqlRoot
  , Channel
  , Content
  ) where

import           Control.Lens           (view)
import           Data.Map               (Map)
import qualified Data.Map               as M (fromList)
import           Data.Set               (Set)
import qualified Data.Set               as S (fromList)
import           Data.Text              (Text, pack)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)

-- MORPHEUS
import           Data.Morpheus.Document (importGQLDocument)
import           Data.Morpheus.Kind     (INPUT_UNION, OBJECT, SCALAR)
import           Data.Morpheus.Types    (Event (..), GQLRootResolver (..), GQLScalar (..), GQLType (..), ID, IOMutRes,
                                         IORes, IOSubRes, Resolver, ScalarValue (..), SubResolver (..), constRes,
                                         mutResolver, resolver)

$(importGQLDocument "examples/Sophisticated/api.gql")

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
          { queryUser = const $ resolver fetchUser
          , queryAnimal = \args -> return (pack $ show (view animal args))
          , querySet = constRes $ S.fromList [1, 2]
          , queryMap = constRes $ M.fromList [("robin", 1), ("carl", 2)]
          , queryWrapped1 = constRes $ A (0, "")
          , queryWrapped2 = constRes $ A ""
          }
    -------------------------------------------------------------
    mutationResolver = return Mutation {mutationCreateAddress, mutationCreateUser}
      where
        mutationCreateUser _ =
          mutResolver
            [Event [UPDATE_USER] (Update {contentID = 12, contentMessage = "some message for user"})]
            fetchUser
        --mutationCreateAddress :: () -> MutRes (Address MutRes)
        mutationCreateAddress _ =
          mutResolver
            [Event [UPDATE_ADDRESS] (Update {contentID = 10, contentMessage = "message for address"})]
            (fetchAddress (Euro 1 0))
    ----------------------------------------------------------------
    subscriptionResolver = return Subscription {subscriptionNewAddress, subscriptionNewUser}
      where
        subscriptionNewUser () = SubResolver {subChannels = [UPDATE_USER], subResolver}
          where
            subResolver (Event _ Update {}) = resolver fetchUser
        subscriptionNewAddress () = SubResolver {subChannels = [UPDATE_ADDRESS], subResolver}
          where
            subResolver (Event _ Update {contentID}) = resolver $ fetchAddress (Euro contentID 0)

fetchAddress :: Monad m => Euro -> m (Either String (Address (Resolver m)))
fetchAddress _ =
  return $ Right Address {addressCity = constRes "", addressStreet = constRes "", addressHouseNumber = constRes 0}

fetchUser :: Monad m => m (Either String (User (Resolver m)))
fetchUser =
  return $
  Right $
  User
    { userName = constRes "George"
    , userEmail = constRes "George@email.com"
    , userAddress = const resolveAddress
    , userOffice = resolveOffice
    , userHome = constRes HH
    , userEntity = constRes $ MyUnionUser unionUser
    }
  where
    unionAddress =
      Address {addressCity = constRes "Hamburg", addressStreet = constRes "Street", addressHouseNumber = constRes 20}
  -- Office
    resolveOffice _officeArgs = resolver $ fetchAddress (Euro 1 1)
    resolveAddress = resolver $ fetchAddress (Euro 1 0)
    unionUser =
      User
        { userName = constRes "David"
        , userEmail = constRes "David@email.com"
        , userAddress = const resolveAddress
        , userOffice = resolveOffice
        , userHome = constRes BLN
        , userEntity = const $ return $ MyUnionAddress unionAddress
        }
