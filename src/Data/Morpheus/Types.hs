-- | GQL Types
module Data.Morpheus.Types
  ( gqlResolver
  , gqlStreamResolver
  -- Resolver Monad
  , Resolver
  , ResM
  , StreamM
  -- Type Classes
  , GQLType(description)
  , GQLScalar(parseValue, serialize)
  -- Values
  , GQLRequest(..)
  , GQLResponse(..)
  , ID(..)
  , ScalarValue(..)
  , GQLRootResolver(..)
  -- Config
  , EventContent
  ) where

import           Data.Morpheus.Types.GQLScalar      (GQLScalar (parseValue, serialize))
import           Data.Morpheus.Types.GQLType        (GQLType (description))
import           Data.Morpheus.Types.ID             (ID (..))
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..))
import           Data.Morpheus.Types.IO             (GQLRequest (..), GQLResponse (..))
import           Data.Morpheus.Types.Resolver       (EventContent, GQLRootResolver (..), ResM, Resolver, StreamM,
                                                     gqlResolver, gqlStreamResolver)
