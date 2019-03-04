{-# LANGUAGE DefaultSignatures , OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables , MultiParamTypeClasses, RankNTypes , DisambiguateRecordFields , FlexibleInstances , FlexibleContexts , TypeOperators #-}

module Data.Morpheus.Generics.GQLQuery
    ( GQLQuery(..)
    )
where


import           Control.Monad
import           Data.Map                       ( insert )
import           Data.Data                      ( Data
                                                , Typeable
                                                , typeOf
                                                , TypeRep
                                                )
import           Data.Text                      ( Text(..)
                                                , pack
                                                )

import           GHC.Generics
import           Data.Morpheus.Types.Types      ( SelectionSet
                                                , QuerySelection(..)
                                                , GQLQueryRoot(..)
                                                , ResolveIO(..)
                                                , failResolveIO
                                                )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..)
                                                , initialMeta
                                                )
import           Data.Morpheus.Generics.GQLArgs ( GQLArgs(..) )
import           Data.Morpheus.Types.Introspection
                                                ( GQL__Type(..)
                                                , GQL__Field
                                                , GQL__TypeKind(..)
                                                , GQL__InputValue
                                                , GQLTypeLib
                                                , GQL__EnumValue
                                                , createType
                                                , createField
                                                , emptyLib
                                                )
import           Data.Morpheus.Generics.TypeRep ( Selectors(..)
                                                , resolveTypes
                                                )
import           Data.Proxy
import           Data.Maybe                     ( fromMaybe )
import           Data.Morpheus.Schema.GQL__Schema
                                                ( initSchema
                                                , GQL__Schema
                                                )
import           Data.Morpheus.Generics.GQLSelection
                                                ( GQLSelection(..) )
import           Data.Morpheus.Generics.DeriveResolvers
                                                ( DeriveResolvers(..)
                                                , resolveBySelection
                                                )


class GQLQuery a where

    encodeQuery :: a -> GQLTypeLib -> QuerySelection  ->  ResolveIO JSType
    default encodeQuery :: ( Generic a, Data a, DeriveResolvers (Rep a) , Show a) => a -> GQLTypeLib -> QuerySelection -> ResolveIO JSType
    encodeQuery rootResolver schema (SelectionSet _ sel) = resolveBySelection sel $ schemaResolver ++ resolvers
      where
        schemaResolver = [("__schema", (`encode` initSchema schema))]
        resolvers = deriveResolvers initialMeta $ from rootResolver

    querySchema :: a -> GQLTypeLib -> GQLTypeLib
    default querySchema :: (Generic a, Data a) => a -> GQLTypeLib -> GQLTypeLib
    querySchema _ = introspectQuery (Proxy :: Proxy a)

    introspectQuery :: Proxy a  -> GQLTypeLib -> GQLTypeLib
    default introspectQuery :: (Show a, Selectors (Rep a) GQL__Field , Typeable a) => Proxy a -> GQLTypeLib -> GQLTypeLib
    introspectQuery _ initialTypes = resolveTypes typeLib stack
       where
         typeLib = introspect (Proxy:: Proxy GQL__Schema) queryType
         queryType = insert "Query" (createType "Query" fields) initialTypes
         fieldTypes  = getFields (Proxy :: Proxy (Rep a))
         stack = map snd fieldTypes
         fields = map fst fieldTypes ++ [ createField "__schema" "__Schema" [] ]
