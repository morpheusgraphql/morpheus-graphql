{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Feature.Input.Object.API
  ( api
  )
where

import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Types            ( GQLRequest
                                                , GQLResponse
                                                , GQLRootResolver(..)
                                                , GQLType(..)
                                                , Undefined(..)
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Morpheus.Kind             ( INPUT )
import           Data.Text                      ( Text
                                                , pack
                                                )


data InputObject = InputObject {
  field :: Text,
  nullableField :: Maybe Int,
  recursive :: Maybe InputObject
} deriving (Generic, Show)

instance GQLType InputObject where
  type KIND InputObject = INPUT

-- types & args
newtype Arg a = Arg
  { value :: a
  } deriving (Generic, Show)

-- query
testRes :: Show a => Applicative m => Arg a -> m Text
testRes Arg { value } = pure $ pack $ show value

-- resolver
newtype Query m = Query
  { input  :: Arg InputObject -> m Text
  } deriving (Generic, GQLType)

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver { queryResolver = Query { input = testRes }
                               , mutationResolver = Undefined
                               , subscriptionResolver = Undefined
                               }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
