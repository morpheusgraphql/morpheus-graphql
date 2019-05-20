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

import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.ObjectRep       (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Types.Error              (ResolveIO)
import           Data.Morpheus.Types.Internal.AST       (ASTOutputField, ASTType (..), ASTTypeLib (..))
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.Query.Selection    (SelectionSet)
import           Data.Proxy
import           Data.Text                              (Text)
import           GHC.Generics

class GQLMutation a where
  encodeMutation :: a -> SelectionSet -> ResolveIO JSType
  default encodeMutation :: (Generic a, DeriveResolvers (Rep a)) =>
    a -> SelectionSet -> ResolveIO JSType
  encodeMutation rootResolver sel = resolveBySelection sel $ deriveResolvers "" $ from rootResolver
  mutationSchema :: a -> ASTTypeLib -> ASTTypeLib
  default mutationSchema :: (ObjectRep (Rep a) (Text, ASTOutputField)) =>
    a -> ASTTypeLib -> ASTTypeLib
  mutationSchema _ initialType = resolveTypes mutationType types'
    where
      mutationType =
        initialType
          { mutation =
              Just ("Mutation", ASTType {typeData = fields', typeName = "Mutation", typeDescription = "Description"})
          }
      (fields', types') = unzip $ getFields (Proxy :: Proxy (Rep a))

instance GQLMutation () where
  encodeMutation _ _ = pure JSNull
  mutationSchema _ = id
