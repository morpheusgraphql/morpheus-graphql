{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Generics.GQLQuery
  ( GQLQuery(..)
  ) where

import           Data.Data                              (Data, Typeable)
import           Data.Map                               (insert)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.GQLSelection    (GQLSelection (..))
import           Data.Morpheus.Generics.TypeRep         (Selectors (..), resolveTypes)
import           Data.Morpheus.Schema.Schema            (Schema, initSchema)
import           Data.Morpheus.Schema.Utils.Utils       (Field, TypeLib, createField, createType)
import           Data.Morpheus.Types.Error              (ResolveIO)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (initialMeta)
import           Data.Morpheus.Types.Query.Selection    (SelectionSet)

import           Data.Proxy
import           GHC.Generics

type UpdateTypes = TypeLib -> TypeLib

class GQLQuery a where
  encodeQuery :: a -> TypeLib -> SelectionSet -> ResolveIO JSType
  default encodeQuery :: (Generic a, Data a, DeriveResolvers (Rep a), Show a) =>
    a -> TypeLib -> SelectionSet -> ResolveIO JSType
  encodeQuery rootResolver schema sel = resolveBySelection sel $ schemaResolver ++ resolvers
    where
      schemaResolver = [("__schema", (`encode` initSchema schema))]
      resolvers = deriveResolvers initialMeta $ from rootResolver
  querySchema :: a -> UpdateTypes
  default querySchema :: (Generic a, Data a) =>
    a -> UpdateTypes
  querySchema _ = introspectQuery (Proxy :: Proxy a)
  introspectQuery :: Proxy a -> UpdateTypes
  default introspectQuery :: (Show a, Selectors (Rep a) Field, Typeable a) =>
    Proxy a -> UpdateTypes
  introspectQuery _ initialTypes = resolveTypes typeLib stack
    where
      typeLib = introspect (Proxy :: Proxy Schema) queryType
      queryType = insert "Query" (createType "Query" fields) initialTypes
      fieldTypes = getFields (Proxy :: Proxy (Rep a))
      stack = map snd fieldTypes
      fields = map fst fieldTypes ++ [createField "__schema" "__Schema" []]
