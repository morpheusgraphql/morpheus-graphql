{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.GQLWrapper
  ( EncodeWrapper (..),
    DecodeWrapper (..),
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.Types.Internal.AST
  ( ValidValue,
    Value (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( LiftOperation,
    ResModel (..),
    Resolver,
    SubscriptionField (..),
  )
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Relude

-- | GraphQL Scalar Serializer
class EncodeWrapper (f :: * -> *) where
  encodeWrapper ::
    (LiftOperation o, Monad m) =>
    (a -> Resolver o e m (ResModel o e m)) ->
    f a ->
    Resolver o e m (ResModel o e m)

-- | GraphQL Wrapper Deserializer
class DecodeWrapper (f :: * -> *) where
  decodeWrapper :: Applicative m => (ValidValue -> m a) -> ValidValue -> m (f a)

withList ::
  ( EncodeWrapper f,
    LiftOperation o,
    Monad m
  ) =>
  (a -> f b) ->
  (b -> Resolver o e m (ResModel o e m)) ->
  a ->
  Resolver o e m (ResModel o e m)
withList f encodeValue = encodeWrapper encodeValue . f

instance EncodeWrapper Maybe where
  encodeWrapper = maybe (pure ResNull)

instance EncodeWrapper [] where
  encodeWrapper encodeValue = fmap ResList . traverse encodeValue

instance EncodeWrapper NonEmpty where
  encodeWrapper = withList NonEmpty.toList

instance EncodeWrapper Vector where
  encodeWrapper = withList Vector.toList

instance EncodeWrapper Set where
  encodeWrapper = withList S.toList

instance EncodeWrapper SubscriptionField where
  encodeWrapper encode (SubscriptionField _ res) = encode res

-- ScalarValue -> Either Text (f a)
instance DecodeWrapper Maybe where
  decodeWrapper _ Null = pure Nothing
  decodeWrapper decode x = Just <$> decode x
