{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators  , ViewPatterns    #-}

module Data.Morpheus.Types.Describer
  ( (::->)(..)
  , EnumOf(..)
  ) where

import           Data.Aeson                (ToJSON (..), Value (Null))
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
  deriving (Generic)

instance Show (a ::-> b) where
  show _ = "Inline"

instance (Data a, Data b) => Data (a ::-> b) where
  gfoldl k z (Some c) = z Some `k` c
  gfoldl k z (Resolver _) = z Some `k` (undefined::b)
  gunfold k z _ = k (z Some)
  toConstr _ = conSome
  dataTypeOf _ = ty_Resolver

conSome :: Constr
conSome = mkConstr ty_Resolver "Some" [] Prefix

ty_Resolver :: DataType
ty_Resolver = mkDataType "Module.Resolver" [conSome]

instance (ToJSON o) => ToJSON (p ::-> o) where
  toJSON (Some o)    = toJSON o
  toJSON Resolver {} = Null -- should not be called at all
