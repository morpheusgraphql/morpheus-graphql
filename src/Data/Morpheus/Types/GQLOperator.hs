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

import           Data.Morpheus.Resolve.Encode                   (_encode)
import           Data.Morpheus.Resolve.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection,
                                                                 resolveBySelectionM)
import           Data.Morpheus.Resolve.Generics.TypeRep         (ObjectRep (..), TypeUpdater, resolveTypes)
import           Data.Morpheus.Resolve.Introspect               (_introspect)
import           Data.Morpheus.Schema.Schema                    (Schema, initSchema)
import           Data.Morpheus.Types.Internal.AST.Selection     (SelectionSet)
import           Data.Morpheus.Types.Internal.Data              (DataOutputField, DataType (..), DataTypeLib (..),
                                                                 initTypeLib)
import           Data.Morpheus.Types.Internal.Validation        (ResolveIO, SchemaValidation)
import           Data.Morpheus.Types.Internal.Value             (Value (..))
import           Data.Morpheus.Types.Resolver                   (WithEffect (..))
import           Data.Proxy
import           Data.Text                                      (Text)
import           GHC.Generics

type QResult = Value

type MResult = WithEffect Value

type Encode a r = a -> SelectionSet -> ResolveIO r

type EncodeCon a r = (Generic a, DeriveResolvers (Rep a) r)

type IntroCon a = (ObjectRep (Rep a) (Text, DataOutputField))

operatorType :: Text -> a -> (Text, DataType a)
operatorType name' fields' =
  (name', DataType {typeData = fields', typeName = name', typeHash = "__.OPERATOR." <> name', typeDescription = ""})

class GQLQuery a where
  encodeQuery :: DataTypeLib -> Encode a QResult
  default encodeQuery :: EncodeCon a QResult =>
    DataTypeLib -> Encode a QResult
  encodeQuery types rootResolver sel = resolveBySelection sel (schemaResolver ++ resolvers)
    where
      schemaResolver = [("__schema", (`_encode` initSchema types))]
      resolvers = deriveResolvers "" $ from rootResolver
  querySchema :: a -> SchemaValidation DataTypeLib
  default querySchema :: IntroCon a =>
    a -> SchemaValidation DataTypeLib
  querySchema _ = resolveTypes queryType (_introspect (Proxy @Schema) : stack')
    where
      queryType = initTypeLib (operatorType "Query" fields')
      (fields', stack') = unzip $ getFields (Proxy @(Rep a))

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
      mutationType = initialType {mutation = Just $ operatorType "Mutation" fields'}
      (fields', types') = unzip $ getFields (Proxy :: Proxy (Rep a))

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
      subscriptionType = initialType {subscription = Just $ operatorType "Subscription" fields'}
      (fields', types') = unzip $ getFields (Proxy :: Proxy (Rep a))

instance GQLMutation () where
  encodeMutation _ _ = pure $ pure Null
  mutationSchema _ = return

instance GQLSubscription () where
  encodeSubscription _ _ = pure $ pure Null
  subscriptionSchema _ = return
