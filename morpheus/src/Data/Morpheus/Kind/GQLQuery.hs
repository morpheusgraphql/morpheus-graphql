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

module Data.Morpheus.Kind.GQLQuery
  ( GQLQuery(..)
  ) where

import           Data.Data                              (Data, Typeable)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.TypeRep         (Selectors (..), resolveTypes)
import           Data.Morpheus.Kind.GQLSelection        (GQLSelection (..))
import           Data.Morpheus.Schema.Internal.Types    (Core (..), GObject (..), LibType (..), ObjectField, TypeLib,
                                                         defineType)
import           Data.Morpheus.Schema.Schema            (Schema, initSchema)
import           Data.Morpheus.Types.Error              (ResolveIO)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (initialMeta)
import           Data.Morpheus.Types.Query.Selection    (SelectionSet)
import           Data.Proxy
import           Data.Text                              (Text)
import           GHC.Generics

type UpdateTypes = TypeLib -> TypeLib

class GQLQuery a where
  encodeQuery :: a -> TypeLib -> SelectionSet -> ResolveIO JSType
  default encodeQuery :: (Generic a, Data a, DeriveResolvers (Rep a), Show a) =>
    a -> TypeLib -> SelectionSet -> ResolveIO JSType
  encodeQuery rootResolver types sel = resolveBySelection sel (schemaResolver ++ resolvers)
    where
      schemaResolver = [("__schema", (`encode` initSchema types))] -- TODO: lazy schema derivation
      resolvers = deriveResolvers initialMeta $ from rootResolver
  querySchema :: a -> UpdateTypes
  default querySchema :: (Generic a, Data a) =>
    a -> UpdateTypes
  querySchema _ = introspectQuery (Proxy @a)
  introspectQuery :: Proxy a -> UpdateTypes
  default introspectQuery :: (Show a, Selectors (Rep a) (Text, ObjectField), Typeable a) =>
    Proxy a -> UpdateTypes
  introspectQuery _ initialTypes = resolveTypes typeLib stack
    where
      typeLib = introspect (Proxy @Schema) queryType
      queryType = defineType ("Query", OutputObject $ GObject fields (Core "Query" "Description")) initialTypes
      fieldTypes = getFields (Proxy @(Rep a))
      stack = map snd fieldTypes
      fields = map fst fieldTypes
