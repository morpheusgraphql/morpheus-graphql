{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Kind.GQLOperator
  ( GQLQuery(..)
  , GQLMutation(..)
  , GQLSubscription(..)
  ) where

import           Data.Morpheus.Generics.DeriveResolvers     (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.ObjectRep           (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Kind.Introspect              (_introspect)
import           Data.Morpheus.Kind.Encoder            (_encode)
import           Data.Morpheus.Schema.Schema                (Schema, initSchema)
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataOutputField, DataType (..), DataTypeLib (..),
                                                             initTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.Resolver               (Result (..))
import           Data.Proxy
import           Data.Text                                  (Text)
import           GHC.Generics

type Encode a s = a -> SelectionSet -> ResolveIO (Result Value)

type EncodeCon a = (Generic a, DeriveResolvers (Rep a))

type IntroCon a = (ObjectRep (Rep a) (Text, DataOutputField))

operatorType :: Text -> a -> (Text, DataType a)
operatorType name' fields' = (name', DataType {typeData = fields', typeName = name', typeDescription = ""})

class GQLQuery a where
  encodeQuery :: DataTypeLib -> Encode a Text
  default encodeQuery :: EncodeCon a =>
    DataTypeLib -> Encode a Text
  encodeQuery types rootResolver sel = resolveBySelection sel (schemaResolver ++ resolvers)
    where
      schemaResolver = [("__schema", (`_encode` initSchema types))]
      resolvers = deriveResolvers "" $ from rootResolver
  querySchema :: a -> DataTypeLib
  default querySchema :: IntroCon a =>
    a -> DataTypeLib
  querySchema _ = resolveTypes typeLib stack'
    where
      typeLib = _introspect (Proxy @Schema) queryType
      queryType = initTypeLib (operatorType "Query" fields')
      (fields', stack') = unzip $ getFields (Proxy @(Rep a))

class GQLMutation a where
  encodeMutation :: Encode a Text
  default encodeMutation :: EncodeCon a =>
    Encode a Text
  encodeMutation rootResolver sel = resolveBySelection sel $ deriveResolvers "" $ from rootResolver
  mutationSchema :: a -> DataTypeLib -> DataTypeLib
  default mutationSchema :: IntroCon a =>
    a -> DataTypeLib -> DataTypeLib
  mutationSchema _ initialType = resolveTypes mutationType types'
    where
      mutationType = initialType {mutation = Just $ operatorType "Mutation" fields'}
      (fields', types') = unzip $ getFields (Proxy :: Proxy (Rep a))

class GQLSubscription a where
  encodeSubscription :: Encode a Text
  default encodeSubscription :: EncodeCon a =>
    Encode a Text
  encodeSubscription rootResolver sel = resolveBySelection sel $ deriveResolvers "" $ from rootResolver
  subscriptionSchema :: a -> DataTypeLib -> DataTypeLib
  default subscriptionSchema :: IntroCon a =>
    a -> DataTypeLib -> DataTypeLib
  subscriptionSchema _ initialType = resolveTypes subscriptionType types'
    where
      subscriptionType = initialType {subscription = Just $ operatorType "Subscription" fields'}
      (fields', types') = unzip $ getFields (Proxy :: Proxy (Rep a))

instance GQLMutation () where
  encodeMutation _ _ = pure $ pure Null
  mutationSchema _ = id

instance GQLSubscription () where
  encodeSubscription _ _ = pure $ pure Null
  subscriptionSchema _ = id
