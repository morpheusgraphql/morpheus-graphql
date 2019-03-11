{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Generics.GQLMutation
  ( GQLMutation(..)
  , NoMutation(..)
  ) where

import           Control.Monad
import           Data.Data                              (Data, TypeRep,
                                                         Typeable, typeOf)
import           Data.List                              (find)
import qualified Data.Map                               as M
import           Data.Text                              (Text (..), pack)

import           Data.Maybe                             (fromMaybe)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..),
                                                         resolveBySelection)
import           Data.Morpheus.Generics.GQLArgs         (GQLArgs (..))
import           Data.Morpheus.Generics.GQLSelection    (GQLSelection (..))
import           Data.Morpheus.Generics.TypeRep         (Selectors (..),
                                                         resolveTypes)
import           Data.Morpheus.Schema.GQL__Schema       (GQL__Schema)
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

class GQLMutation a where
  encodeMutation :: a -> GQLTypeLib -> QuerySelection -> ResolveIO JSType
  default encodeMutation :: (Generic a, Data a, DeriveResolvers (Rep a), Show a) =>
    a -> GQLTypeLib -> QuerySelection -> ResolveIO JSType
  encodeMutation rootResolver schema (SelectionSet _ sel pos) =
    resolveBySelection sel $ deriveResolvers initialMeta $ from rootResolver
  mutationSchema :: a -> GQLTypeLib
  default mutationSchema :: (Generic a, Data a) =>
    a -> GQLTypeLib
  mutationSchema _ = introspectMutation (Proxy :: Proxy a)
  introspectMutation :: Proxy a -> GQLTypeLib
  default introspectMutation :: (Show a, Selectors (Rep a) GQL__Field, Typeable a) =>
    Proxy a -> GQLTypeLib
  introspectMutation _ = resolveTypes mutationType types
    where
      mutationType = M.fromList [("Mutation", createType "Mutation" fields)]
      fieldTypes = getFields (Proxy :: Proxy (Rep a))
      types = map snd fieldTypes
      fields = map fst fieldTypes

data NoMutation =
  NoMutation

instance GQLMutation NoMutation where
  encodeMutation _ _ _ = pure JSNull
  mutationSchema _ = emptyLib
  introspectMutation _ = emptyLib
