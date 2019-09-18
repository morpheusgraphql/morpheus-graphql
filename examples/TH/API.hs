{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module TH.API
  ( thApi
  ) where

import           Control.Lens               (view)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

-- MORPHEUS
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocument)
import           Data.Morpheus.Kind         (SCALAR)
import           Data.Morpheus.Types        (GQLRootResolver (..), GQLScalar (..), GQLType (..), ID, IORes,
                                             ScalarValue (..), constRes)

importGQLDocument "./examples/TH/api.gql"

newtype Euro =
  Euro Int
  deriving (Show, Generic)

instance GQLType Euro where
  type KIND Euro = SCALAR

instance GQLScalar Euro where
  parseValue _ = pure (Euro 0)
  serialize (Euro x) = Int (x * 100)

gqlRoot :: GQLRootResolver IO () () (Query IORes) () ()
gqlRoot = GQLRootResolver {queryResolver, mutationResolver = return (), subscriptionResolver = return ()}
  where
    queryResolver :: IORes (Query IORes)
    queryResolver = return Query {queryUser}
      where
        queryUser :: () -> IORes (User IORes)
        queryUser _ =
          return
            User
              { userName = constRes "David"
              , userEmail = constRes "David@email.com"
              , userAddress
              , userHome = constRes HH
              , userMyUnion = constRes $ MyUnionAddress $ simpleAddress "boo"
              }
        userAddress :: AddressArgs -> IORes (Address IORes)
        userAddress args = return $ simpleAddress ""
        --(view (zipCode . uid) args)
        --------------------------------
        simpleAddress streetID =
          Address
            { addressCity = const $ pure "Hamburg"
            , addressStreet = const $ pure streetID
            , addressHouseNumber = const $ pure 20
            }

thApi :: B.ByteString -> IO B.ByteString
thApi = interpreter gqlRoot
