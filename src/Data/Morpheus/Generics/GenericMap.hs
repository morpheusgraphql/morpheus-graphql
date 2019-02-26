{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances , FlexibleContexts , TypeOperators #-}

module Data.Morpheus.Generics.GenericMap ( GenericMap(..) ) where

import           GHC.Generics
import qualified Data.Maybe     as       M
import qualified Data.Text      as       T
import Data.Morpheus.Types.Types (SelectionSet, ResolveIO)
import Data.Morpheus.Types.MetaInfo (MetaInfo(..))
import Data.Morpheus.Types.JSType (JSType)


-- type D1 = M1 D
-- type C1 = M1 C
-- type S1 = M1 S
-- M1 : Meta-information (constructor names, etc.)
-- D  :Datatype : Class for dataTypes that represent dataTypes
-- C :Constructor :
-- S - Selector: Class for dataTypes that represent records
-- Rep = D1 (...)  (C1 ...) (S1 (...) :+: D1 (...)  (C1 ...) (S1 (...)

class GenericMap f where
    encodeFields:: MetaInfo -> SelectionSet -> f a -> [(T.Text, ResolveIO JSType)]

instance GenericMap U1  where
    encodeFields _ _  _ = []

instance (Selector s, GenericMap f) => GenericMap (M1 S s f) where
    encodeFields meta gql m@(M1 src) = encodeFields (meta{ key = T.pack $ selName m}) gql src

instance (Datatype c, GenericMap f) => GenericMap (M1 D c f)  where
    encodeFields meta gql m@(M1 src) = encodeFields (meta{ className = T.pack $ datatypeName m}) gql src

instance (Constructor c  , GenericMap f) => GenericMap (M1 C c f)  where
    encodeFields meta gql m@(M1 src) =  encodeFields (meta{ cons = T.pack $ conName m}) gql src

instance (GenericMap f , GenericMap g ) => GenericMap (f :*: g)  where
    encodeFields meta gql  (a :*: b) = encodeFields meta gql a ++ encodeFields meta gql b
