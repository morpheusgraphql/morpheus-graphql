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

import           Data.Morpheus.Generics.DeriveResolvers     (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.ObjectRep           (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataOutputField, DataType (..), DataTypeLib (..))
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Proxy
import           Data.Text                                  (Text)
import           GHC.Generics

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

instance GQLMutation () where
  encodeMutation _ _ = pure Null
  mutationSchema _ = id
