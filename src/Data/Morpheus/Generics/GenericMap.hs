{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances , FlexibleContexts , TypeOperators #-}

module Data.Morpheus.Generics.GenericMap
    ( GenericMap(..)
    , getField
    , initMeta
    )
where

import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text(..)
                                                , pack
                                                )
import           GHC.Generics
import           Data.Morpheus.Types.Types     ( QuerySelection(..)
                                                , SelectionSet
                                                , EvalIO(..)
                                                , MetaInfo(..)
                                                , JSType(..)
                                                , Eval(..)
                                                )

getField :: MetaInfo -> SelectionSet -> Eval QuerySelection
getField meta gql = pure $ fromMaybe QNull (lookup (key meta) gql)

-- type D1 = M1 D
-- type C1 = M1 C
-- type S1 = M1 S
-- M1 : Meta-information (constructor names, etc.)
-- D  :Datatype : Class for dataTypes that represent dataTypes
-- C :Constructor :
-- S - Selector: Class for dataTypes that represent records
-- Rep = D1 (...)  (C1 ...) (S1 (...) :+: D1 (...)  (C1 ...) (S1 (...)

initMeta = MetaInfo { className = "", cons = "", key = "" }

class GenericMap f where
    encodeFields:: MetaInfo -> SelectionSet -> (f a) -> [(Text, EvalIO JSType)]

instance GenericMap U1  where
    encodeFields _ _  _ = []

instance (Selector s, GenericMap f) => GenericMap (M1 S s f) where
    encodeFields meta gql m@(M1 src) = encodeFields (meta{ key = pack $ selName m}) gql src

instance (Datatype c, GenericMap f) => GenericMap (M1 D c f)  where
    encodeFields meta gql m@(M1 src) = encodeFields (meta{ className = pack $ datatypeName m}) gql src

instance (Constructor c  , GenericMap f) => GenericMap (M1 C c f)  where
    encodeFields meta gql m@(M1 src) =  encodeFields (meta{ cons = pack $ conName m}) gql src

instance (GenericMap f , GenericMap g ) => GenericMap (f :*: g)  where
    encodeFields meta gql  (a :*: b) = (encodeFields meta gql a) ++ (encodeFields meta gql b)
