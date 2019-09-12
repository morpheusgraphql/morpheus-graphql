{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
                                             ScalarValue (..))

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
    queryResolver = return Query {user}
      where
        user :: () -> IORes (User IORes)
        user _ =
          return
            User
              { name = const $ pure "David"
              , email = const $ pure "David@email.com"
              , address
              , home = const $ pure HH
              , myUnion = const $ pure $ MyUnionAddress $ simpleAddress "boo"
              }
        address :: AddressArgs -> IORes (Address IORes)
        address args = return $ simpleAddress (view (zipCode . uid) args)
        --------------------------------
        simpleAddress streetID =
          Address {city = const $ pure "Hamburg", street = const $ pure streetID, houseNumber = const $ pure 20}

thApi :: B.ByteString -> IO B.ByteString
thApi = interpreter gqlRoot
