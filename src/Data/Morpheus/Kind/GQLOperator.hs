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
import           Data.Morpheus.Kind.OutputRouter            (_encode, _introspect)
import           Data.Morpheus.Schema.Schema                (Schema, initSchema)
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataOutputField, DataType (..), DataTypeLib (..),
                                                             initTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Proxy
import           Data.Text                                  (Text)
import           GHC.Generics

class GQLQuery a where
  encodeQuery :: DataTypeLib -> a -> SelectionSet -> ResolveIO Value
  default encodeQuery :: (Generic a, DeriveResolvers (Rep a)) =>
    DataTypeLib -> a -> SelectionSet -> ResolveIO Value
  encodeQuery types rootResolver sel = resolveBySelection sel (schemaResolver ++ resolvers)
    where
      schemaResolver = [("__schema", (`_encode` initSchema types))]
      resolvers = deriveResolvers "" $ from rootResolver
  querySchema :: a -> DataTypeLib
  default querySchema :: (ObjectRep (Rep a) (Text, DataOutputField)) =>
    a -> DataTypeLib
  querySchema _ = resolveTypes typeLib stack'
    where
      typeLib = _introspect (Proxy @Schema) queryType
      queryType = initTypeLib ("Query", DataType {typeData = fields', typeName = "Query", typeDescription = ""})
      (fields', stack') = unzip $ getFields (Proxy @(Rep a))

class GQLMutation a where
  encodeMutation :: a -> SelectionSet -> ResolveIO Value
  default encodeMutation :: (Generic a, DeriveResolvers (Rep a)) =>
    a -> SelectionSet -> ResolveIO Value
  encodeMutation rootResolver sel = resolveBySelection sel $ deriveResolvers "" $ from rootResolver
  mutationSchema :: a -> DataTypeLib -> DataTypeLib
  default mutationSchema :: (ObjectRep (Rep a) (Text, DataOutputField)) =>
    a -> DataTypeLib -> DataTypeLib
  mutationSchema _ initialType = resolveTypes mutationType types'
    where
      mutationType =
        initialType
          { mutation =
              Just ("Mutation", DataType {typeData = fields', typeName = "Mutation", typeDescription = "Description"})
          }
      (fields', types') = unzip $ getFields (Proxy :: Proxy (Rep a))

class GQLSubscription a where
  encodeSubscription :: a -> SelectionSet -> ResolveIO Value
  default encodeSubscription :: (Generic a, DeriveResolvers (Rep a)) =>
    a -> SelectionSet -> ResolveIO Value
  encodeSubscription rootResolver sel = resolveBySelection sel $ deriveResolvers "" $ from rootResolver
  subscriptionSchema :: a -> DataTypeLib -> DataTypeLib
  default subscriptionSchema :: (ObjectRep (Rep a) (Text, DataOutputField)) =>
    a -> DataTypeLib -> DataTypeLib
  subscriptionSchema _ initialType = resolveTypes subscriptionType types'
    where
      subscriptionType =
        initialType
          { subscription =
              Just ("Subscription", DataType {typeData = fields', typeName = "Subscription", typeDescription = ""})
          }
      (fields', types') = unzip $ getFields (Proxy :: Proxy (Rep a))

instance GQLMutation () where
  encodeMutation _ _ = pure Null
  mutationSchema _ = id

instance GQLSubscription () where
  encodeSubscription _ _ = pure Null
  subscriptionSchema _ = id
