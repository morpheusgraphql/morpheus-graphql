-- | GQL Types
module Data.Morpheus.Types
  ( ScalarValue(..)
  , (::->)
  , (::->>)
  , (:->)(..)
  , ID(..)
  , GQLType(description)
  , GQLRootResolver(..)
  , GQLScalar(parseValue, serialize)
  , GQLRequest(..)
  , GQLResponse(..)
  , GQLMutation
  , GQLQuery
  , GQLSubscription
  , withEffect
  ) where

import           Data.Morpheus.Types.GQLOperator    (GQLMutation, GQLQuery, GQLSubscription)
import           Data.Morpheus.Types.GQLScalar      (GQLScalar (parseValue, serialize))
import           Data.Morpheus.Types.GQLType        (GQLType (description))
import           Data.Morpheus.Types.ID             (ID (..))
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..))
import           Data.Morpheus.Types.Request        (GQLRequest (..))
import           Data.Morpheus.Types.Resolver       ((:->) (..), (::->), (::->>), WithEffect (..))
import           Data.Morpheus.Types.Response       (GQLResponse (..))
import           Data.Morpheus.Types.Types          (GQLRootResolver (..))

-- | used in mutation or subscription resolver , adds effect to normal resolver
withEffect :: [c] -> Either String a -> Either String (WithEffect c a)
withEffect channels v = WithEffect channels <$> v
