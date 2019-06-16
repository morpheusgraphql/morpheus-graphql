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

module Data.Morpheus.Types.GQLOperator
  ( GQLQuery(..)
  , GQLMutation(..)
  , GQLSubscription(..)
  ) where

import           Data.Morpheus.Resolve.Encode                   (encode)
import           Data.Morpheus.Resolve.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection,
                                                                 resolveBySelectionM)
import           Data.Morpheus.Resolve.Generics.TypeRep         (ObjectRep (..), TypeUpdater, resolveTypes)
import           Data.Morpheus.Resolve.Internal                 (Context (..), OutputOf)
import           Data.Morpheus.Resolve.Introspect               (introspect)
import           Data.Morpheus.Schema.Schema                    (Schema, initSchema)
import           Data.Morpheus.Types.Internal.AST.Selection     (SelectionSet)
import           Data.Morpheus.Types.Internal.Data              (DataArguments, DataType (..), DataTypeLib (..),
                                                                 initTypeLib)
import           Data.Morpheus.Types.Internal.Validation        (ResolveIO, SchemaValidation)
import           Data.Morpheus.Types.Internal.Value             (Value (..))
import           Data.Morpheus.Types.Resolver                   (WithEffect (..))
import           Data.Proxy
import           Data.Text                                      (Text)
import           Data.Typeable                                  (Typeable, typeRep, typeRepFingerprint)
import           GHC.Generics

type QResult = Value

type MResult = WithEffect Value

type Encode a r = a -> SelectionSet -> ResolveIO r

type EncodeCon a r = (Generic a, DeriveResolvers (Rep a) r)

type IntroCon a = (ObjectRep (Rep a) DataArguments, Typeable a)

operatorType :: Typeable t => Proxy t -> Text -> a -> (Text, DataType a)
operatorType proxy name' fields' =
  ( name'
  , DataType
      {typeData = fields', typeName = name', typeFingerprint = typeRepFingerprint $ typeRep proxy, typeDescription = ""})

class GQLQuery a where
  encodeQuery :: DataTypeLib -> Encode a QResult
  default encodeQuery :: EncodeCon a QResult =>
    DataTypeLib -> Encode a QResult
  encodeQuery types rootResolver sel = resolveBySelection sel (schemaResolver ++ resolvers)
    where
      schemaResolver = [("__schema", encode (initSchema types))]
      resolvers = deriveResolvers "" $ from rootResolver
  querySchema :: a -> SchemaValidation DataTypeLib
  default querySchema :: IntroCon a =>
    a -> SchemaValidation DataTypeLib
  querySchema _ = resolveTypes queryType (introspect (Context :: OutputOf Schema) : stack')
    where
      queryType = initTypeLib (operatorType (Proxy @a) "Query" fields')
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

class GQLMutation a where
  encodeMutation :: Encode a MResult
  default encodeMutation :: EncodeCon a MResult =>
    Encode a MResult
  encodeMutation rootResolver sel = resolveBySelectionM sel $ deriveResolvers "" $ from rootResolver
  mutationSchema :: a -> TypeUpdater
  default mutationSchema :: IntroCon a =>
    a -> TypeUpdater
  mutationSchema _ initialType = resolveTypes mutationType types'
    where
      mutationType = initialType {mutation = Just $ operatorType (Proxy @a) "Mutation" fields'}
      (fields', types') = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

class GQLSubscription a where
  encodeSubscription :: Encode a MResult
  default encodeSubscription :: EncodeCon a MResult =>
    Encode a MResult
  encodeSubscription rootResolver sel = resolveBySelectionM sel $ deriveResolvers "" $ from rootResolver
  subscriptionSchema :: a -> TypeUpdater
  default subscriptionSchema :: IntroCon a =>
    a -> TypeUpdater
  subscriptionSchema _ initialType = resolveTypes subscriptionType types'
    where
      subscriptionType = initialType {subscription = Just $ operatorType (Proxy @a) "Subscription" fields'}
      (fields', types') = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

instance GQLMutation () where
  encodeMutation _ _ = pure $ pure Null
  mutationSchema _ = return

instance GQLSubscription () where
  encodeSubscription _ _ = pure $ pure Null
  subscriptionSchema _ = return
