{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Kind.GQLSubscription
  ( GQLSubscription(..)
  ) where

import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.ObjectRep       (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Schema.Internal.AST      (Core (..), GObject (..), ObjectField, TypeLib (..))
import           Data.Morpheus.Types.Error              (ResolveIO)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.Query.Selection    (SelectionSet)
import           Data.Proxy
import           Data.Text                              (Text)
import           GHC.Generics

class GQLSubscription a where
  encodeSubscription :: a -> SelectionSet -> ResolveIO JSType
  default encodeSubscription :: (Generic a, DeriveResolvers (Rep a)) =>
    a -> SelectionSet -> ResolveIO JSType
  encodeSubscription rootResolver sel = resolveBySelection sel $ deriveResolvers "" $ from rootResolver
  subscriptionSchema :: a -> TypeLib -> TypeLib
  default subscriptionSchema :: (ObjectRep (Rep a) (Text, ObjectField)) =>
    a -> TypeLib -> TypeLib
  subscriptionSchema _ initialType = resolveTypes subscriptionType types
    where
      subscriptionType =
        initialType {subscription = Just ("Subscription", GObject fields $ Core "Subscription" "Description")}
      fieldTypes = getFields (Proxy :: Proxy (Rep a))
      types = map snd fieldTypes
      fields = map fst fieldTypes

instance GQLSubscription () where
  encodeSubscription _ _ = pure JSNull
  subscriptionSchema _ = id
