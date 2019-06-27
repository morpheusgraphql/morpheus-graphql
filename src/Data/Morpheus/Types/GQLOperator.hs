{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Types.GQLOperator
  ( GQLQuery(..)
  , GQLMutation(..)
  , GQLSubscription(..)
  ) where

import           Data.Morpheus.Resolve.Encode               (ObjectFieldResolvers (..), resolveBySelection, resolversBy)
import           Data.Morpheus.Resolve.Generics.TypeRep     (ObjectRep (..), TypeUpdater, resolveTypes)
import           Data.Morpheus.Schema.SchemaAPI             (hiddenRootFields, schemaAPI, schemaTypes)
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataArguments, DataFingerprint (..), DataType (..),
                                                             DataTypeLib (..), initTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (ResolveT, SchemaValidation)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.Resolver               (EffectT (..))
import           Data.Proxy
import           Data.Text                                  (Text)
import           Data.Typeable                              (Typeable)
import           GHC.Generics

type Encode m a = a -> SelectionSet -> ResolveT m Value

type EncodeCon m a = (Generic a, ObjectFieldResolvers (Rep a) m)

type IntroCon a = (ObjectRep (Rep a) DataArguments, Typeable a)

operatorType :: Text -> a -> (Text, DataType a)
operatorType name' fields' =
  ( name'
  , DataType {typeData = fields', typeName = name', typeFingerprint = SystemFingerprint name', typeDescription = ""})

-- | derives GQL Query Operator
class GQLQuery a where
  encodeQuery :: DataTypeLib -> Encode IO a
  default encodeQuery :: EncodeCon IO a =>
    DataTypeLib -> Encode IO a
  encodeQuery types rootResolver sel =
    resolveBySelection sel (resolversBy (schemaAPI types) ++ resolversBy rootResolver)
  querySchema :: a -> SchemaValidation DataTypeLib
  default querySchema :: IntroCon a =>
    a -> SchemaValidation DataTypeLib
  querySchema _ = resolveTypes queryType (schemaTypes : types)
    where
      queryType = initTypeLib (operatorType "Query" (hiddenRootFields ++ fields))
      (fields, types) = unzip $ objectFieldTypes (Proxy @(Rep a))

-- | derives GQL Subscription Mutation
class GQLMutation a where
  encodeMutation :: Encode (EffectT IO Text) a
  default encodeMutation :: EncodeCon (EffectT IO Text) a =>
    Encode (EffectT IO Text) a
  encodeMutation rootResolver sel = resolveBySelection sel $ resolversBy rootResolver
  mutationSchema :: a -> TypeUpdater
  default mutationSchema :: IntroCon a =>
    a -> TypeUpdater
  mutationSchema _ initialType = resolveTypes mutationType types'
    where
      mutationType = initialType {mutation = Just $ operatorType "Mutation" fields'}
      (fields', types') = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

-- | derives GQL Subscription Operator
class GQLSubscription a where
  encodeSubscription :: Encode (EffectT IO Text) a
  default encodeSubscription :: EncodeCon (EffectT IO Text) a =>
    Encode (EffectT IO Text) a
  encodeSubscription rootResolver sel = resolveBySelection sel $ resolversBy rootResolver
  subscriptionSchema :: a -> TypeUpdater
  default subscriptionSchema :: IntroCon a =>
    a -> TypeUpdater
  subscriptionSchema _ initialType = resolveTypes subscriptionType types'
    where
      subscriptionType = initialType {subscription = Just $ operatorType "Subscription" fields'}
      (fields', types') = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

instance GQLMutation () where
  encodeMutation _ _ = pure Null
  mutationSchema _ = return

instance GQLSubscription () where
  encodeSubscription _ _ = pure Null
  subscriptionSchema _ = return
