{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass , TypeOperators #-}

module Data.Morpheus.Types.Types
    ( Eval
    , QuerySelection(..)
    , SelectionSet
    , (::->)(..)
    , MetaInfo(..)
    , JSType(..)
    , GQLQueryRoot(..)
    , Fragment(..)
    , FragmentLib
    , GQLResponse(..)
    , GQLRequest(..)
    , Argument(..)
    , EvalIO(..)
    , failEvalIO
    , Arguments
    , EnumOf(..)
    )
where

import           Prelude                 hiding ( filter )
import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import           Data.Map                       ( Map
                                                , filter
                                                , mapKeys
                                                )
import           Data.Aeson                     ( ToJSON(..)
                                                , object
                                                , (.=)
                                                , FromJSON(..)
                                                , Value(Null)
                                                )
import           Data.Data
import           Data.Morpheus.Types.Error     ( GQLError )
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

newtype EnumOf a = EnumOf { unpackEnum :: a }  deriving (Show, Generic , Data);

failEvalIO :: [GQLError] -> EvalIO a
failEvalIO = ExceptT . pure . Left

data JSType =  JSObject (Map Text JSType)| JSList [JSType] |  JSEnum Text | JSInt Int | JSBool Bool | JSString Text | JSNull  deriving (Show, Generic );

data Argument =  Variable Text | Argument JSType deriving (Show, Generic);

type Arguments = [(Text,Argument)]

type SelectionSet  = [(Text,QuerySelection)]

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

replaceType "_type" = "type"
replaceType x = x

instance ToJSON JSType where
    toJSON (JSInt x) = toJSON x
    toJSON (JSBool x) = toJSON x
    toJSON (JSString x) = toJSON x
    toJSON JSNull = Null
    toJSON (JSObject x) = toJSON (mapKeys replaceType x)
    toJSON (JSList x) = toJSON x

data GQLResponse = Data JSType | Errors [GQLError]  deriving (Show,Generic) ;

instance ToJSON  GQLResponse where
  toJSON (Errors _errors) = object ["errors" .= _errors];
  toJSON (Data _data) = object ["data" .= _data];

data GQLRequest = GQLRequest {
    query:: Text
    ,operationName:: Maybe Text
    ,variables:: Maybe (Map Text Text)
} deriving (Show,Generic,ToJSON,FromJSON)
