{-# LANGUAGE DataKinds #-}
-- | GQL Types
module Data.Morpheus.Types
  ( resolver
  -- Resolver Monad
  , Resolver
  , Event(..)
  -- Type Classes
  , GQLType(KIND, description)
  , GQLScalar(parseValue, serialize)
  -- Values
  , GQLRequest(..)
  , GQLResponse(..)
  , ID(..)
  , ScalarValue(..)
  , GQLRootResolver(..)
  , constRes
  , Undefined(..)
  , SubResolver
  , MutResolver
  , GADTResolver(..)
  , GraphQLT(..)
  , OperationKind(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  ) where

import           Data.Morpheus.Types.GQLScalar         (GQLScalar (parseValue, serialize))
import           Data.Morpheus.Types.GQLType           (GQLType (KIND, description))
import           Data.Morpheus.Types.ID                (ID (..))
import           Data.Morpheus.Types.Internal.Data     (MUTATION, OperationKind (..), QUERY, SUBSCRIPTION)
import           Data.Morpheus.Types.Internal.Resolver (Event (..), GADTResolver (..), GQLRootResolver (..),
                                                        GraphQLT (..), MutResolver, Resolver, resolver)
import           Data.Morpheus.Types.Internal.Value    (ScalarValue (..))
import           Data.Morpheus.Types.IO                (GQLRequest (..), GQLResponse (..))
import           Data.Morpheus.Types.Types             (Undefined (..))

type SubResolver = GADTResolver SUBSCRIPTION

-- resolves constant value on any argument
constRes :: Monad m => b -> a -> GADTResolver QUERY m e b
constRes = const . pure
