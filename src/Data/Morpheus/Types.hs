-- | GQL Types
module Data.Morpheus.Types
  ( resolver
  , mutResolver
  , toMutResolver
  -- Resolver Monad
  , IORes
  , IOMutRes
  , IOSubRes
  , Resolver
  , SubRootRes
  , SubResolver(..)
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
  ) where

import           Data.Morpheus.Types.GQLScalar      (GQLScalar (parseValue, serialize))
import           Data.Morpheus.Types.GQLType        (GQLType (KIND, description))
import           Data.Morpheus.Types.ID             (ID (..))
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..))
import           Data.Morpheus.Types.IO             (GQLRequest (..), GQLResponse (..))
import           Data.Morpheus.Types.Resolver       (Event (..), GQLRootResolver (..), MutResolver, Resolver,
                                                     SubResolver (..), SubRootRes, mutResolver, resolver, toMutResolver)

-- resolves constant value on any argument
constRes :: Monad m => b -> a -> m b
constRes = const . return

type IORes = Resolver IO

type IOMutRes e c = MutResolver IO e c

type IOSubRes e c = SubResolver IO e c
