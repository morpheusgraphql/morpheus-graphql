-- | GQL Types
module Data.Morpheus.Types
  ( ScalarValue(..)
  , (::->)
  , (::->>)
  , Resolver(..)
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
  , addEffect
  , EffectT
  ) where

import           Data.Morpheus.Types.GQLOperator    (GQLMutation, GQLQuery, GQLSubscription)
import           Data.Morpheus.Types.GQLScalar      (GQLScalar (parseValue, serialize))
import           Data.Morpheus.Types.GQLType        (GQLType (description))
import           Data.Morpheus.Types.ID             (ID (..))
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..))
import           Data.Morpheus.Types.Request        (GQLRequest (..))
import           Data.Morpheus.Types.Resolver       ((::->), (::->>), Effect (..), EffectT (..), Resolver (..))
import           Data.Morpheus.Types.Response       (GQLResponse (..))
import           Data.Morpheus.Types.Types          (GQLRootResolver (..))

-- | used in mutation or subscription resolver , adds effect to normal resolver
withEffect :: Monad m => [c] -> m a -> EffectT m c a
withEffect channels = EffectT . fmap (Effect channels)

addEffect :: Monad m => [c] -> EffectT m c a -> EffectT m c a
addEffect channels EffectT {runEffectT = monadEffect} = EffectT $ insertEffect <$> monadEffect
  where
    insertEffect x = x {resultEffects = channels ++ resultEffects x}
