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

import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.ObjectRep       (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Kind.OutputRouter        (_encode, _introspect)
import           Data.Morpheus.Schema.Schema            (Schema, initSchema)
import           Data.Morpheus.Types.Internal.Validation              (ResolveIO)
import           Data.Morpheus.Types.Internal.Data      (DataOutputField, DataType (..), DataTypeLib (..), initTypeLib)
import           Data.Morpheus.Types.Internal.Value     (Value (..))
import           Data.Morpheus.Types.Internal.AST.Selection    (SelectionSet)
import           Data.Proxy
import           Data.Text                              (Text)
import           GHC.Generics

class GQLQuery a where
  encodeQuery :: a -> DataTypeLib -> SelectionSet -> ResolveIO Value
  default encodeQuery :: (Generic a, DeriveResolvers (Rep a)) =>
    a -> DataTypeLib -> SelectionSet -> ResolveIO Value
  encodeQuery rootResolver types sel = resolveBySelection sel (schemaResolver ++ resolvers)
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
