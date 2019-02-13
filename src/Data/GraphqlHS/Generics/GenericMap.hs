{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances , FlexibleContexts , TypeOperators #-}

module Data.GraphqlHS.Generics.GenericMap
    ( GenericMap(..)
    , getField
    , initMeta
    )
where

import           Data.Maybe                     ( fromMaybe )
import           Prelude                 hiding ( lookup )
import           Data.Text                      ( Text(..)
                                                , pack
                                                )
import           Data.Map                       ( Map
                                                , lookup
                                                , singleton
                                                , fromList
                                                , union
                                                )
import           GHC.Generics
import           Data.GraphqlHS.Types.Types     ( QuerySelection(..)
                                                , SelectionSet
                                                , EvalIO(..)
                                                , GQLPrimitive(JSNull)
                                                , MetaInfo(..)
                                                , GQLType(..)
                                                , Eval(..)
                                                )

getField :: MetaInfo -> (Map Text QuerySelection) -> Eval QuerySelection
getField meta gql = pure $ fromMaybe QNull (lookup (key meta) gql)

-- type D1 = M1 D
-- type C1 = M1 C
-- type S1 = M1 S
-- M1 : Meta-information (constructor names, etc.)
-- D  :Datatype : Class for datatypes that represent datatypes
-- C :Constructor :
-- S - Selector: Class for datatypes that represent records
-- Rep = D1 (...)  (C1 ...) (S1 (...) :+: D1 (...)  (C1 ...) (S1 (...)

initMeta = MetaInfo { className = "", cons = "", key = "" }

class GenericMap f where
    transform:: MetaInfo -> SelectionSet -> (f a) -> [(Text, EvalIO GQLType)]

instance GenericMap U1  where
    transform _ _  _ = []

instance (Selector s, GenericMap f) => GenericMap (M1 S s f) where
    transform meta gql m@(M1 src) = transform (meta{ key = pack $ selName m}) gql src

instance (Datatype c, GenericMap f) => GenericMap (M1 D c f)  where
    transform meta gql m@(M1 src) = transform (meta{ className = pack $ datatypeName m}) gql src

instance (Constructor c  , GenericMap f) => GenericMap (M1 C c f)  where
    transform meta gql m@(M1 src) =  transform (meta{ cons = pack $ conName m}) gql src

instance (GenericMap f , GenericMap g ) => GenericMap (f :*: g)  where
    transform meta gql  (a :*: b) = (transform meta gql a) ++ (transform meta gql b)
