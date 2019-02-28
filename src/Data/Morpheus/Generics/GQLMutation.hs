{-# LANGUAGE DefaultSignatures , OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables , MultiParamTypeClasses, RankNTypes , DisambiguateRecordFields , FlexibleInstances , FlexibleContexts , TypeOperators #-}

module Data.Morpheus.Generics.GQLMutation (GQLMutation(..),NoMutation(..)) where


import           Control.Monad
import           Data.List                      ( find )
import           Data.Data                      ( Data
                                                , Typeable
                                                , typeOf
                                                , TypeRep
                                                )
import           Data.Text                      ( Text(..)
                                                , pack
                                                )
import qualified Data.Map                      as M

import           GHC.Generics
import           Data.Morpheus.Types.Types      ( SelectionSet
                                                , QuerySelection(..)
                                                , GQLQueryRoot(..)
                                                , ResolveIO(..)
                                                , failResolveIO
                                                )
import Data.Morpheus.Types.JSType (JSType(..))
import Data.Morpheus.Types.MetaInfo (MetaInfo(..), initialMeta)
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
import           Data.Morpheus.Generics.TypeRep ( Selectors(..) , resolveTypes )
import           Data.Proxy
import           Data.Morpheus.Generics.GenericMap ( GenericMap(..) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Morpheus.Schema.GQL__Schema
                                                ( initSchema
                                                , GQL__Schema
                                                )
import           Data.Morpheus.Generics.GQLSelection
                                                ( GQLSelection(..)
                                                , wrapAsObject
                                                )
import           Data.Morpheus.PreProcess       ( preProcessQuery )

setProperty :: Text -> JSType -> JSType -> JSType
setProperty name prop (JSObject obj) = JSObject (M.insert name prop obj)

getProperty :: Text -> QuerySelection -> Maybe QuerySelection
getProperty name (SelectionSet _ sel) = lookup name sel

unpackObj (SelectionSet _ sel) = sel

class GQLMutation a where

    encodeMutation :: a -> GQLQueryRoot  ->  ResolveIO JSType
    default encodeMutation :: ( Generic a, Data a, GenericMap (Rep a) , Show a) => a -> GQLQueryRoot -> ResolveIO JSType
    encodeMutation rootResolver query =  case preProcessQuery schema query of
        Right validGQL -> wrapAsObject $ encodeFields initialMeta (unpackObj validGQL) $ from rootResolver
        Left x ->  failResolveIO x
        where schema = introspectMutation (Proxy :: Proxy a);

    mutationSchema :: a -> GQLTypeLib
    default mutationSchema :: (Generic a, Data a) => a -> GQLTypeLib
    mutationSchema _ = introspectMutation (Proxy :: Proxy a)

    introspectMutation :: Proxy a  -> GQLTypeLib
    default introspectMutation :: (Show a, Selectors (Rep a) GQL__Field , Typeable a) => Proxy a -> GQLTypeLib
    introspectMutation _ = resolveTypes mutationType types
       where
         mutationType = M.fromList [("Mutation", createType "Mutation" fields)]
         fieldTypes  = getFields (Proxy :: Proxy (Rep a))
         types = map snd fieldTypes
         fields = map fst fieldTypes

data NoMutation = NoMutation


instance GQLMutation NoMutation where
  encodeMutation _ _ = pure JSNull
  mutationSchema _  = emptyLib
  introspectMutation _ = emptyLib
