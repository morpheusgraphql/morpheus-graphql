{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.GQLWrapper
  ( EncodeWrapper (..),
    DecodeWrapper (..),
    DecodeWrapperConstraint,
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
import qualified Data.Set as Set
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

type family DecodeWrapperConstraint (f :: * -> *) a :: Constraint where
  DecodeWrapperConstraint Set a = (Ord a)
  DecodeWrapperConstraint f a = ()

-- | GraphQL Wrapper Deserializer
class DecodeWrapper (f :: * -> *) where
  decodeWrapper ::
    (Monad m, DecodeWrapperConstraint f a) =>
    (ValidValue -> m a) ->
    ValidValue ->
    ExceptT Message m (f a)

instance DecodeWrapper Maybe where
  decodeWrapper _ Null = pure Nothing
  decodeWrapper decode x = Just <$> lift (decode x)

instance DecodeWrapper [] where
  decodeWrapper decode (List li) = lift $ traverse decode li
  decodeWrapper _ isType = ExceptT $ pure $ Left (typeMismatch "List" isType)

instance DecodeWrapper NonEmpty where
  decodeWrapper = withRefinedList (maybe (Left "Expected a NonEmpty list") Right . NonEmpty.nonEmpty)

instance DecodeWrapper Seq where
  decodeWrapper decode = fmap Seq.fromList . decodeWrapper decode

instance DecodeWrapper Vector where
  decodeWrapper decode = fmap Vector.fromList . decodeWrapper decode

instance DecodeWrapper Set where
  decodeWrapper decode value = do
    listVal <- decodeWrapper decode value
    haveSameSize (Set.fromList listVal) listVal

haveSameSize ::
  ( Foldable l,
    Monad m
  ) =>
  Set a ->
  l b ->
  ExceptT Message m (Set a)
haveSameSize setVal listVal
  | length setVal == length listVal = pure setVal
  | otherwise = ExceptT $ pure $ Left (fromString ("Expected a List without duplicates, found " <> show (length listVal - length listVal) <> " duplicates"))

withRefinedList ::
  Monad m =>
  ([a] -> Either Message (rList a)) ->
  (ValidValue -> m a) ->
  ValidValue ->
  ExceptT Message m (rList a)
withRefinedList refiner decode (List li) = do
  listRes <- lift (traverse decode li)
  case refiner listRes of
    Left err -> ExceptT $ pure $ Left (typeMismatch err (List li))
    Right value -> pure value
withRefinedList _ _ isType = ExceptT $ pure $ Left (typeMismatch "List" isType)

-- if value is already validated but value has different type
typeMismatch :: Message -> Value s -> Message
typeMismatch text jsType =
  "Type mismatch! expected:" <> msg text <> ", got: "
    <> msg jsType
