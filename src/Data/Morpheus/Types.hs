{-# LANGUAGE DataKinds #-}
-- | GQL Types
module Data.Morpheus.Types
  ( Event(..)
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
  , constMutRes
  , Undefined(..)
  , Res
  , MutRes
  , SubRes
  , IORes
  , IOMutRes
  , IOSubRes
  , Resolver(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  ) where

import           Data.Morpheus.Types.GQLScalar         (GQLScalar (parseValue, serialize))
import           Data.Morpheus.Types.GQLType           (GQLType (KIND, description))
import           Data.Morpheus.Types.ID                (ID (..))
import           Data.Morpheus.Types.Internal.Data     (MUTATION, QUERY, SUBSCRIPTION)
import           Data.Morpheus.Types.Internal.Resolver (Event (..), GQLRootResolver (..), PureOperation, Resolver (..))
import           Data.Morpheus.Types.Internal.Value    (ScalarValue (..))
import           Data.Morpheus.Types.IO                (GQLRequest (..), GQLResponse (..))
import           Data.Morpheus.Types.Types             (Undefined (..))

type Res = Resolver QUERY
type MutRes = Resolver MUTATION
type SubRes = Resolver SUBSCRIPTION

type IORes = Res IO
type IOMutRes = MutRes IO
type IOSubRes = SubRes IO

-- resolves constant value on any argument
constRes :: (PureOperation o ,Monad m) => b -> a -> Resolver o m e b
constRes = const . pure

constMutRes :: Monad m =>  [e] -> a -> args -> MutRes m e a
constMutRes list value = const $ MutationResolver list $ pure value
