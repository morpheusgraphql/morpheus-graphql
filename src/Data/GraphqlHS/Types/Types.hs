{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass , TypeOperators #-}

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
    , EvalIO(..)
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
import           Control.Monad                  ( join )
import           Control.Monad.Trans            ( liftIO )
import           Control.Monad                  ( forM
                                                , liftM
                                                )


data Eval a = Fail [GQLError] | Val a deriving (Generic) ;

data EvalIO a = IOVal (IO a) | IOFail [GQLError] deriving (Generic)


instance Functor EvalIO where
    fmap f (IOVal v) =  IOVal $ v >>= \x -> pure (f x)
    fmap f (IOFail x)  = IOFail x

instance Applicative EvalIO where
    pure  = IOVal . pure
    (<*>) (IOVal f1) (IOVal f2) = IOVal ( f1 >>= \x-> ( x <$> f2) )
    (<*>) (IOFail x) _ = IOFail x

instance Monad EvalIO where
    return = IOVal . pure
    (>>=) (IOVal xm) fm = IOVal $ do
        x <- xm
        case (fm x) of
            IOVal v -> v

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


data a ::-> b = TypeHolder (Maybe a) | Resolver (a -> EvalIO b) | Some b | None deriving (Generic)

instance Show (a ::-> b) where
    show _ = "Inline"

instance (Data a, Data b) => Data (a ::-> b) where
    gfoldl k z _ = z None
    gunfold k z c = z None
    toConstr (Some _ ) = con_Some
    toConstr _      = con_None
    dataTypeOf _ = ty_Resolver

con_Some = mkConstr ty_Resolver "Some" [] Prefix
con_None = mkConstr ty_Resolver "None" [] Prefix
ty_Resolver = mkDataType "Module.Resolver" [con_None, con_Some]


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
