-- | GQL Types
module Data.Morpheus.Types
  ( ScalarValue(..)
  , (::->)
  , (::->>)
  , Resolver(..)
  , ID(..)
  , GQLType(description)
  , GQLRootResolver(..)
  , GQLScalar(parseValue,serialize)
  , GQLRequest(..)
  , GQLResponse(..)
  , GQLArgs
  , GQLMutation
  , GQLQuery
  , GQLSubscription
  , withEffect
  ) where

import           Data.Morpheus.Types.GQLArgs        (GQLArgs)
import           Data.Morpheus.Types.GQLOperator    (GQLMutation, GQLQuery, GQLSubscription)
import           Data.Morpheus.Types.GQLScalar      (GQLScalar (..))
import           Data.Morpheus.Types.GQLType        (GQLType (..))
import           Data.Morpheus.Types.ID             (ID (..))
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..))
import           Data.Morpheus.Types.Request        (GQLRequest (..))
import           Data.Morpheus.Types.Resolver       ((::->), (::->>), Resolver (..), WithEffect (..))
import           Data.Morpheus.Types.Response       (GQLResponse (..))
import           Data.Morpheus.Types.Types          (GQLRootResolver (..))
import           Data.Text                          (Text)

-- | used in mutation or subscription resolver , adds effect to normal resolver
withEffect :: [Text] -> Either String a -> Either String (WithEffect a)
withEffect channels v = WithEffect channels <$> v
