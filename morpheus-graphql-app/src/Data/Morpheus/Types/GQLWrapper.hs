{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.GQLWrapper
  ( EncodeWrapper (..),
    DecodeWrapper (..),
    DecodeWrapperConstraint,
    EncodeWrapperValue (..),
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.App.Internal.Resolving
  ( ResolverValue,
    SubscriptionField (..),
    mkList,
    mkNull,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    GQLError,
    ValidValue,
    Value (..),
    msg,
  )
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Relude

-- | GraphQL Wrapper Serializer
class EncodeWrapper (wrapper :: Type -> Type) where
  encodeWrapper ::
    (Monad m) =>
    (a -> m (ResolverValue m)) ->
    wrapper a ->
    m (ResolverValue m)

withList :: (Monad m, Foldable f) => (a -> m (ResolverValue m)) -> f a -> m (ResolverValue m)
withList encodeValue = encodeWrapper encodeValue . toList

instance EncodeWrapper Maybe where
  encodeWrapper = maybe (pure mkNull)

instance EncodeWrapper [] where
  encodeWrapper encodeValue = fmap mkList . traverse encodeValue

instance EncodeWrapper NonEmpty where
  encodeWrapper = withList

instance EncodeWrapper Seq where
  encodeWrapper = withList

instance EncodeWrapper Vector where
  encodeWrapper = withList

instance EncodeWrapper Set where
  encodeWrapper = withList

instance EncodeWrapper SubscriptionField where
  encodeWrapper encode (SubscriptionField _ res) = encode res

type family DecodeWrapperConstraint (f :: Type -> Type) a :: Constraint where
  DecodeWrapperConstraint Set a = (Ord a)
  DecodeWrapperConstraint f a = ()

-- | GraphQL Wrapper Deserializer
class DecodeWrapper (f :: Type -> Type) where
  decodeWrapper ::
    (Monad m, DecodeWrapperConstraint f a) =>
    (ValidValue -> m a) ->
    ValidValue ->
    ExceptT GQLError m (f a)

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
  ExceptT GQLError m (Set a)
haveSameSize setVal listVal
  | length setVal == length listVal = pure setVal
  | otherwise = ExceptT $ pure $ Left (fromString ("Expected a List without duplicates, found " <> show (length listVal - length setVal) <> " duplicates"))

withRefinedList ::
  (Monad m) =>
  ([a] -> Either GQLError (rList a)) ->
  (ValidValue -> m a) ->
  ValidValue ->
  ExceptT GQLError m (rList a)
withRefinedList refiner decode (List li) = do
  listRes <- lift (traverse decode li)
  case refiner listRes of
    Left err -> ExceptT $ pure $ Left (typeMismatch err (List li))
    Right value -> pure value
withRefinedList _ _ isType = ExceptT $ pure $ Left (typeMismatch "List" isType)

-- if value is already validated but value has different type
typeMismatch :: GQLError -> Value s -> GQLError
typeMismatch text jsType =
  "Type mismatch! expected:"
    <> msg text
    <> ", got: "
    <> msg jsType

class EncodeWrapperValue (f :: Type -> Type) where
  encodeWrapperValue :: (Monad m) => (a -> m (Value CONST)) -> f a -> m (Value CONST)

instance EncodeWrapperValue Maybe where
  encodeWrapperValue = maybe (pure Null)

instance EncodeWrapperValue [] where
  encodeWrapperValue f xs = List <$> traverse f xs

instance EncodeWrapperValue Set where
  encodeWrapperValue f = encodeWrapperValue f . toList

instance EncodeWrapperValue NonEmpty where
  encodeWrapperValue f = encodeWrapperValue f . toList

instance EncodeWrapperValue Seq where
  encodeWrapperValue f = encodeWrapperValue f . toList

instance EncodeWrapperValue Vector where
  encodeWrapperValue f = encodeWrapperValue f . toList
