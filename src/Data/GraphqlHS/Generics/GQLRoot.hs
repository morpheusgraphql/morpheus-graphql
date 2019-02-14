{-# LANGUAGE DefaultSignatures , OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables , MultiParamTypeClasses, RankNTypes , DisambiguateRecordFields , FlexibleInstances , FlexibleContexts , TypeOperators #-}

module Data.GraphqlHS.Generics.GQLRoot
    ( GQLRoot(..)
    )
where

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
import           Data.GraphqlHS.Types.Types     ( SelectionSet
                                                , QuerySelection(..)
                                                , Eval(..)
                                                , MetaInfo(..)
                                                , GQLType(..)
                                                , GQLQueryRoot(..)
                                                , EvalIO(..)
                                                , failEvalIO
                                                )
import           Data.GraphqlHS.ErrorMessage    ( handleError
                                                , subfieldsNotSelected
                                                )
import           Data.GraphqlHS.Generics.GQLArgs
                                                ( GQLArgs(..) )
import           Data.GraphqlHS.Types.Introspection
                                                ( GQL__Type(..)
                                                , GQL__Field
                                                , GQL__TypeKind(..)
                                                , GQL__InputValue
                                                , GQLTypeLib
                                                , GQL__Deprication__Args
                                                , GQL__EnumValue
                                                , createType
                                                , createField
                                                , emptyLib
                                                )
import           Data.GraphqlHS.Generics.TypeRep
                                                ( Selectors(..) )
import           Data.Proxy
import           Data.GraphqlHS.Generics.GenericMap
                                                ( GenericMap(..)
                                                , getField
                                                , initMeta
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.GraphqlHS.Schema.GQL__Schema
                                                ( initSchema
                                                , GQL__Schema
                                                )
import           Data.GraphqlHS.Generics.GQLSelection
                                                ( GQLSelection(..)
                                                , wrapAsObject
                                                , arrayMap
                                                )
import           Data.GraphqlHS.PreProcess      ( preProccessQuery )

addProp :: GQLType -> GQLType -> GQLType
addProp prop (Obj obj) = Obj (M.insert "__schema" prop obj)

unpackObj (SelectionSet _ x) = x

class GQLRoot a where

    encode :: a -> GQLQueryRoot  ->  EvalIO GQLType
    default encode :: ( Generic a, Data a, GenericMap (Rep a) , Show a) => a -> GQLQueryRoot -> EvalIO GQLType
    encode rootValue gqlRoot =  case preProccessQuery schema gqlRoot of
        Right validGQL -> case (lookup "__schema" (unpackObj validGQL)) of
            Nothing -> responce
            Just x ->  (liftM2 addProp) (item x) responce
            where
                item (SelectionSet _ x) = wrapAsObject (encodeFields initMeta x (from $ initSchema $ M.elems $ schema))
                responce = wrapAsObject $ encodeFields initMeta (unpackObj validGQL) (from rootValue)
        Left x ->  failEvalIO x
        where
            schema = introspectRoot (Proxy :: Proxy a);

    introspectRoot :: Proxy a  -> GQLTypeLib
    default introspectRoot :: (Show a, Selectors (Rep a) , Typeable a) => Proxy a -> GQLTypeLib
    introspectRoot _ = do
        let typeLib = introspect (Proxy:: Proxy GQL__Schema) emptyLib
        arrayMap (M.insert "Query" (createType "Query" fields) typeLib) stack
               where
                   fieldTypes  = getFields (Proxy :: Proxy (Rep a))
                   stack = (map snd fieldTypes)
                   fields = map fst fieldTypes ++ [ createField "__schema" "GQL__Schema" [] ]
