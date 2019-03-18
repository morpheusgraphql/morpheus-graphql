{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Morpheus.Types.Describer
  ( (::->)(..)
  , EnumOf(..)
  ) where

import           Data.Aeson                (FromJSON (..), ToJSON (..), Value (Null))
import           Data.Data                 (Constr, Data, DataType, Fixity (Prefix), dataTypeOf,
                                            gfoldl, gunfold, mkConstr, mkDataType, toConstr)
import           Data.Morpheus.Types.Error (ResolveIO)
import           GHC.Generics              (Generic)

newtype EnumOf a = EnumOf
  { unpackEnum :: a
  } deriving (Show, Generic, Data)

data a ::-> b
  = Resolver (a -> ResolveIO b)
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
  toJSON (Some o)    = toJSON o
  toJSON None        = Null
  toJSON Resolver {} = Null -- should not be called at all
