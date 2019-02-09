{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass , DeriveDataTypeable , TypeOperators #-}

module Data.GraphqlHS.Types.Types
    ( GQLPrimitive(..)
    , Eval(..)
    , GQLValue(..)
    , Object
    , Head(..)
    , (::->)(..)
    , MetaInfo(..)
    , GQLType(..)
    , GQLQueryRoot(..)
    , Fragment(..)
    , FragmentLib
    , GQLResponce
    , GQLRequest(..)
    , Arg(..)
    , InlineResolver(InlineResolver)
    )
where

import           Prelude                 hiding ( filter )
import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import           Data.Map                       ( Map
                                                , filter
                                                )
import           Data.Aeson                     ( ToJSON(..)
                                                , object
                                                , (.=)
                                                , FromJSON(..)
                                                , Value(Null)
                                                )
import           Data.Data
import           Data.GraphqlHS.Types.Error     ( GQLError )


data Eval a = Fail [GQLError] | Val a deriving (Generic) ;

instance Functor Eval where
    fmap f (Val x) = Val (f x)
    fmap f (Fail x) = Fail x

instance Applicative Eval where
    pure = Val
    (<*>) (Val f) x = fmap f x
    (<*>) (Fail x) _ = Fail x

instance Monad Eval where
    (>>=) (Val x) fm = fm x
    (>>=) (Fail x) _ = Fail x

instance (ToJSON a, Generic a) => ToJSON (Eval a) where
    toJSON (Fail errors) = object ["errors" .= errors];
    toJSON (Val d) = object ["data" .= d];

data GQLPrimitive = JSEnum Text | JSInt Int | JSBool Bool | JSString Text | JSNull  deriving (Show, Generic);

data Arg =  Var Text |ArgValue GQLPrimitive deriving (Show, Generic);

instance ToJSON GQLPrimitive where
    toJSON (JSInt x) = toJSON x
    toJSON (JSBool x) = toJSON x
    toJSON (JSString x) = toJSON x
    toJSON JSNull = Null

type Args = Map Text Arg ;

data Head = Head Args | Empty deriving (Show, Generic);

type Object  = Map Text GQLValue;

data GQLValue =
    Query Head GQLValue |
    Object Object |
    Field Text |
    Spread Text |
    QNull
    deriving (Show, Generic);

type FragmentLib = Map Text Fragment;

data Fragment = Fragment {
    id:: Text ,
    target :: Text ,
    fragmentContent:: GQLValue
} deriving (Show, Generic)


data GQLQueryRoot = GQLQueryRoot {
    fragments:: FragmentLib,
    queryBody :: GQLValue
}

data InlineResolver a b = InlineResolver (a -> IO b) | EmptyLambda deriving (Generic);

instance Show (InlineResolver a b) where
    show _ = "InlineResolver"

instance (Data a, Data b) => Data (InlineResolver a b) where
    gfoldl k z (InlineResolver f) = z EmptyLambda
    gfoldl k z EmptyLambda = z EmptyLambda

    gunfold k z c = case constrIndex c of
                                 1 -> z EmptyLambda
                                 2 -> z EmptyLambda
    toConstr (InlineResolver _ ) = con_LambdaField
    toConstr EmptyLambda      = con_EmptyLambda
    dataTypeOf _ = ty_LambdaField

con_LambdaField = mkConstr ty_LambdaField "LambdaField" [] Prefix
con_EmptyLambda = mkConstr ty_LambdaField "EmptyLambda" [] Prefix
ty_LambdaField =
    mkDataType "Module.LambdaField" [con_LambdaField, con_EmptyLambda]

data p ::-> o = Resolve (Maybe p) (Maybe o) | Inline (InlineResolver p o) | Some o | None deriving (Show , Generic , Data )

instance FromJSON ( p ::->  o) where
    parseJSON _ =  pure None


instance (ToJSON o) => ToJSON ( p ::->  o) where
    toJSON (Some o) = toJSON o
    toJSON None = Null

data MetaInfo = MetaInfo {
        className ::  Text,
        cons::  Text,
        key ::  Text
};

data GQLType =  Obj (Map Text GQLType)| Li [GQLType] | Prim GQLPrimitive deriving (Show, Generic );

instance ToJSON GQLType where
    toJSON (Prim x) = toJSON x
    toJSON (Obj x) = toJSON x
    toJSON (Li x) = toJSON x

type GQLResponce = Eval GQLType;

data GQLRequest = GQLRequest {
    query:: Text
    ,operationName:: Maybe Text
    ,variables:: Maybe (Map Text Text)
} deriving (Show,Generic,ToJSON,FromJSON)
