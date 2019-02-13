{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass , TypeOperators #-}

module Data.GraphqlHS.Types.Types
    ( GQLPrimitive(..)
    , Eval(..)
    , QuerySelection(..)
    , SelectionSet
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
    , failEvalIO
    , Arguments
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
import           Control.Monad.Trans            ( liftIO
                                                , lift
                                                , MonadTrans
                                                )
import           Control.Monad                  ( forM
                                                , liftM
                                                )
import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , runExceptT
                                                )

type Eval a = Either [GQLError] a ;
type EvalIO  = ExceptT [GQLError] IO;

failEvalIO :: [GQLError] -> EvalIO a
failEvalIO = ExceptT . pure . Left

--instance (ToJSON a, Generic a) => ToJSON (Eval a) where
--    toJSON (Fail errors) = object ["errors" .= errors];
--    toJSON (Val d) = object ["data" .= d];

data GQLPrimitive = JSEnum Text | JSInt Int | JSBool Bool | JSString Text | JSNull  deriving (Show, Generic);

data Arg =  Var Text | ArgValue GQLPrimitive deriving (Show, Generic);

instance ToJSON GQLPrimitive where
    toJSON (JSInt x) = toJSON x
    toJSON (JSBool x) = toJSON x
    toJSON (JSString x) = toJSON x
    toJSON JSNull = Null

type Arguments = [(Text,Arg)];
type SelectionSet  = Map Text QuerySelection;

data QuerySelection =
    SelectionSet Arguments SelectionSet |
    Field Arguments Text |
    Spread Text |
    QNull
    deriving (Show, Generic);

type FragmentLib = Map Text Fragment;

data Fragment = Fragment {
    id:: Text ,
    target :: Text ,
    fragmentContent:: QuerySelection
} deriving (Show, Generic)


data GQLQueryRoot = GQLQueryRoot {
    fragments:: FragmentLib,
    queryBody :: QuerySelection,
    inputVariables:: Map Text Text
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
