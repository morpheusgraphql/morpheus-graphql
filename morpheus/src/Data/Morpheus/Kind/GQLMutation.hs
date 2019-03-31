{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Kind.GQLMutation
  ( GQLMutation(..)
  ) where

import           Data.Data                              (Data, Typeable)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.TypeRep         (Selectors (..), resolveTypes)
import           Data.Morpheus.Schema.Internal.Types    (Core (..), GObject (..), LibType (..), ObjectField, TypeLib,
                                                         defineType, initTypeLib)
import           Data.Morpheus.Types.Error              (ResolveIO)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (initialMeta)
import           Data.Morpheus.Types.Query.Selection    (SelectionSet)
import           Data.Proxy
import           Data.Text                              (Text)
import           GHC.Generics

class GQLMutation a where
  encodeMutation :: a -> SelectionSet -> ResolveIO JSType
  default encodeMutation :: (Generic a, Data a, DeriveResolvers (Rep a), Show a) =>
    a -> SelectionSet -> ResolveIO JSType
  encodeMutation rootResolver sel = resolveBySelection sel $ deriveResolvers initialMeta $ from rootResolver
  mutationSchema :: a -> TypeLib
  default mutationSchema :: (Generic a, Data a) =>
    a -> TypeLib
  mutationSchema _ = introspectMutation (Proxy :: Proxy a)
  introspectMutation :: Proxy a -> TypeLib
  default introspectMutation :: (Show a, Selectors (Rep a) (Text, ObjectField), Typeable a) =>
    Proxy a -> TypeLib
  introspectMutation _ = resolveTypes mutationType types
    where
      mutationType = defineType ("Mutation", OutputObject $ GObject fields $ Core "Mutation" "Description") initTypeLib
      fieldTypes = getFields (Proxy :: Proxy (Rep a))
      types = map snd fieldTypes
      fields = map fst fieldTypes

instance GQLMutation () where
  encodeMutation _ _ = pure JSNull
  mutationSchema _ = initTypeLib
  introspectMutation _ = initTypeLib
