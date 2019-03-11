{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Morpheus.Types.Types
  ( Validation
  , QuerySelection(..)
  , SelectionSet
  , (::->)(..)
  , GQLQueryRoot(..)
  , Fragment(..)
  , FragmentLib
  , GQLResponse(..)
  , GQLRequest(..)
  , Argument(..)
  , ResolveIO
  , failResolveIO
  , Arguments
  , EnumOf(..)
  , GQLOperator(..)
  ) where

import           Control.Monad.Trans.Except   (ExceptT (..))
import           Data.Aeson                   (FromJSON (..), ToJSON (..), Value (Null), pairs,
                                               (.=))
import           Data.Data
import           Data.Map                     (Map)
import           Data.Morpheus.Types.Error    (GQLErrors, JSONError (..))
import           Data.Morpheus.Types.JSType   (JSType)
import           Data.Morpheus.Types.MetaInfo (Position)
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)

type Key = Text

type ResolveIO = ExceptT GQLErrors IO

newtype EnumOf a = EnumOf
  { unpackEnum :: a
  } deriving (Show, Generic, Data)

failResolveIO :: GQLErrors -> ResolveIO a
failResolveIO = ExceptT . pure . Left

data Argument
  = Variable Key
             Position
  | Argument JSType
             Position
  deriving (Show)

type Arguments = [(Key, Argument)]

type Validation a = Either GQLErrors a

type SelectionSet = [(Key, QuerySelection)]

data QuerySelection
  = SelectionSet Arguments
                 SelectionSet
                 Position
  | Field Arguments
          Key
          Position
  | Spread Key
           Position
  | QNull
  deriving (Show)

data GQLOperator
  = QueryOperator Text
                  QuerySelection
  | MutationOperator Text
                     QuerySelection

type FragmentLib = Map Text Fragment

data Fragment = Fragment
  { id              :: Text
  , target          :: Text
  , fragmentContent :: QuerySelection
  } deriving (Show)

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , queryBody      :: GQLOperator
  , inputVariables :: Map Text JSType
  }

data a ::-> b
  = TypeHolder (Maybe a)
  | Resolver (a -> ResolveIO b)
  | Some b
  | None
  deriving (Generic)

instance Show (a ::-> b) where
  show _ = "Inline"

instance (Data a, Data b) => Data (a ::-> b) where
  gfoldl _ z _ = z None
  gunfold _ z _ = z None
  toConstr (Some _) = con_Some
  toConstr _        = con_None
  dataTypeOf _ = ty_Resolver

con_Some :: Constr
con_Some = mkConstr ty_Resolver "Some" [] Prefix

con_None :: Constr
con_None = mkConstr ty_Resolver "None" [] Prefix

ty_Resolver :: DataType
ty_Resolver = mkDataType "Module.Resolver" [con_None, con_Some]

instance FromJSON (p ::-> o) where
  parseJSON _ = pure None

instance (ToJSON o) => ToJSON (p ::-> o) where
  toJSON (Some o) = toJSON o
  toJSON None     = Null

data GQLResponse
  = Data JSType
  | Errors [JSONError]
  deriving (Show, Generic)

instance ToJSON GQLResponse where
  toEncoding (Data _data)     = pairs $ "data" .= _data
  toEncoding (Errors _errors) = pairs $ "errors" .= _errors

data GQLRequest = GQLRequest
  { query         :: Text
  , operationName :: Maybe Text
  , variables     :: Maybe (Map Text JSType)
  } deriving (Show, Generic, FromJSON)
