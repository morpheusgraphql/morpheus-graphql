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

import           Control.Monad
import           Data.Data                              (Data, TypeRep,
                                                         Typeable, typeOf)
import           Data.Map                               (insert)
import           Data.Text                              (Text (..), pack)

import           Data.Maybe                             (fromMaybe)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..),
                                                         resolveBySelection)
import           Data.Morpheus.Generics.GQLSelection    (GQLSelection (..))
import           Data.Morpheus.Generics.TypeRep         (Selectors (..),
                                                         resolveTypes)
import           Data.Morpheus.Schema.GQL__Schema       (GQL__Schema,
                                                         initSchema)
import           Data.Morpheus.Types.Introspection      (GQLTypeLib,
                                                         GQL__EnumValue,
                                                         GQL__Field,
                                                         GQL__InputValue,
                                                         GQL__Type (..),
                                                         GQL__TypeKind (..),
                                                         createField,
                                                         createType, emptyLib)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (MetaInfo (..),
                                                         initialMeta)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..),
                                                         QuerySelection (..),
                                                         ResolveIO (..),
                                                         SelectionSet,
                                                         failResolveIO)
import           Data.Proxy
import           GHC.Generics

class GQLQuery a where
  encodeQuery :: a -> GQLTypeLib -> QuerySelection -> ResolveIO JSType
  default encodeQuery :: (Generic a, Data a, DeriveResolvers (Rep a), Show a) =>
    a -> GQLTypeLib -> QuerySelection -> ResolveIO JSType
  encodeQuery rootResolver schema (SelectionSet _ sel pos) = resolveBySelection sel $ schemaResolver ++ resolvers
    where
      schemaResolver = [("__schema", (`encode` initSchema schema))]
      resolvers = deriveResolvers initialMeta $ from rootResolver
  querySchema :: a -> GQLTypeLib -> GQLTypeLib
  default querySchema :: (Generic a, Data a) =>
    a -> GQLTypeLib -> GQLTypeLib
  querySchema _ = introspectQuery (Proxy :: Proxy a)
  introspectQuery :: Proxy a -> GQLTypeLib -> GQLTypeLib
  default introspectQuery :: (Show a, Selectors (Rep a) GQL__Field, Typeable a) =>
    Proxy a -> GQLTypeLib -> GQLTypeLib
  introspectQuery _ initialTypes = resolveTypes typeLib stack
    where
      typeLib = introspect (Proxy :: Proxy GQL__Schema) queryType
      queryType = insert "Query" (createType "Query" fields) initialTypes
      fieldTypes = getFields (Proxy :: Proxy (Rep a))
      stack = map snd fieldTypes
      fields = map fst fieldTypes ++ [createField "__schema" "__Schema" []]
