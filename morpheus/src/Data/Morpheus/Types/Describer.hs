{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Morpheus.Types.Describer
  ( (::->)(..)
  , EnumOf(..)
  ) where

import           Data.Data                 (Constr, Data, DataType, Fixity (Prefix), dataTypeOf,
                                            gfoldl, gunfold, mkConstr, mkDataType, toConstr)
import           Data.Morpheus.Types.Error (ResolveIO)
import           GHC.Generics              (Generic)

newtype EnumOf a = EnumOf
  { unpackEnum :: a
  } deriving (Show, Generic, Data)

data a ::-> b
  = Resolver (a -> ResolveIO b)
  | Resolved b
  deriving (Generic)

instance Show (a ::-> b) where
  show _ = "Resolver"

instance (Data a, Data b) => Data (a ::-> b) where
  gfoldl k z (Resolved c) = z Resolved `k` c
  gfoldl k z (Resolver _) = z Resolved `k` (undefined :: b)
  gunfold k z _ = k (z Resolved)
  toConstr _ = conResolved
  dataTypeOf _ = tyResolver

conResolved :: Constr
conResolved = mkConstr tyResolver "Resolved" [] Prefix

tyResolver :: DataType
tyResolver = mkDataType "Module.Resolver" [conResolved]
