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
import           Data.Morpheus.Types.Error              (ResolveIO)
import           Data.Morpheus.Types.Internal.AST       (ASTOutputField, ASTType (..), ASTTypeLib (..))
import           Data.Morpheus.Types.Internal.Value     (Value (..))
import           Data.Morpheus.Types.Query.Selection    (SelectionSet)
import           Data.Proxy
import           Data.Text                              (Text)
import           GHC.Generics

class GQLSubscription a where
  encodeSubscription :: a -> SelectionSet -> ResolveIO Value
  default encodeSubscription :: (Generic a, DeriveResolvers (Rep a)) =>
    a -> SelectionSet -> ResolveIO Value
  encodeSubscription rootResolver sel = resolveBySelection sel $ deriveResolvers "" $ from rootResolver
  subscriptionSchema :: a -> ASTTypeLib -> ASTTypeLib
  default subscriptionSchema :: (ObjectRep (Rep a) (Text, ASTOutputField)) =>
    a -> ASTTypeLib -> ASTTypeLib
  subscriptionSchema _ initialType = resolveTypes subscriptionType types'
    where
      subscriptionType =
        initialType
          { subscription =
              Just ("Subscription", ASTType {typeData = fields', typeName = "Subscription", typeDescription = ""})
          }
      (fields', types') = unzip $ getFields (Proxy :: Proxy (Rep a))

instance GQLSubscription () where
  encodeSubscription _ _ = pure JSNull
  subscriptionSchema _ = id
