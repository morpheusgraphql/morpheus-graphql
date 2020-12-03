{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.GQLWrapper
  ( EncodeWrapper (..),
    DecodeWrapper (..),
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.Types.Internal.AST
  ( Message,
    ValidValue,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( LiftOperation,
    ResModel (..),
    Resolver,
    SubscriptionField (..),
  )
import qualified Data.Sequence as Seq
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Relude

-- | GraphQL Wrapper Serializer
class EncodeWrapper (f :: * -> *) where
  encodeWrapper ::
    (LiftOperation o, Monad m) =>
    (a -> Resolver o e m (ResModel o e m)) ->
    f a ->
    Resolver o e m (ResModel o e m)

-- | GraphQL Wrapper Deserializer
class DecodeWrapper (f :: * -> *) where
  decodeWrapper :: Monad m => (ValidValue -> m a) -> ValidValue -> m (Either Message (f a))

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
  encodeWrapper = withList toList

instance EncodeWrapper Seq where
  encodeWrapper = withList toList

instance EncodeWrapper Vector where
  encodeWrapper = withList toList

instance EncodeWrapper Set where
  encodeWrapper = withList toList

instance EncodeWrapper SubscriptionField where
  encodeWrapper encode (SubscriptionField _ res) = encode res

-- ScalarValue -> Either Text (f a)
instance DecodeWrapper Maybe where
  decodeWrapper _ Null = pure $ pure Nothing
  decodeWrapper decode x = pure . Just <$> decode x

instance DecodeWrapper [] where
  decodeWrapper decode (List li) = pure <$> traverse decode li
  decodeWrapper _ isType = pure $ Left (typeMismatch "List" isType)

instance DecodeWrapper NonEmpty where
  decodeWrapper = withRefinedList (maybe (Left "Expected a NonEmpty list") Right . NonEmpty.nonEmpty)

instance DecodeWrapper Seq where
  decodeWrapper decode = fmap (Seq.fromList <$>) . decodeWrapper decode

instance DecodeWrapper Vector where
  decodeWrapper decode = fmap (Vector.fromList <$>) . decodeWrapper decode

withRefinedList ::
  Monad m =>
  ([a] -> Either Message (rList a)) ->
  (ValidValue -> m a) ->
  ValidValue ->
  m (Either Message (rList a))
withRefinedList refiner decode (List li) = do
  listRes <- traverse decode li
  case refiner listRes of
    Left err -> pure $ Left (typeMismatch err (List li))
    Right value -> pure $ Right value
withRefinedList _ _ isType = pure $ Left (typeMismatch "List" isType)

-- if value is already validated but value has different type
typeMismatch :: Message -> Value s -> Message
typeMismatch text jsType =
  "Type mismatch! expected:" <> msg text <> ", got: "
    <> msg jsType
