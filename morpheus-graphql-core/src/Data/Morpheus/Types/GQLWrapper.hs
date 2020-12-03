{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.GQLWrapper
  ( EncodeWrapper (..),
    DecodeWrapper (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( ValidValue,
    Value (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( LiftOperation,
    ResModel (..),
    Resolver,
  )
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

instance EncodeWrapper Maybe where
  encodeWrapper = maybe (pure ResNull)

-- ScalarValue -> Either Text (f a)
instance DecodeWrapper Maybe where
  decodeWrapper _ Null = pure Nothing
  decodeWrapper decode x = Just <$> decode x
