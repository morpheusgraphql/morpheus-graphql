{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Morpheus.Types.Describer
  ( (::->)(..)
  , Deprecation(..)
  , EnumOf(..)
  ) where

import           Data.Data                 (Constr, Data, DataType, Fixity (Prefix), dataTypeOf,
                                            gfoldl, gunfold, mkConstr, mkDataType, toConstr)
import           Data.Morpheus.Types.Error (ResolveIO)
import           GHC.Generics              (Generic)

newtype EnumOf a = EnumOf
  { unpackEnum :: a
  } deriving (Show, Generic, Data)

newtype Deprecation a = Deprecation
  { unpackDeprecation :: a
  } deriving (Show, Generic, Data)

newtype a ::-> b =
  Resolver (a -> ResolveIO b)
  deriving (Generic)

instance Show (a ::-> b) where
  show _ = "Resolver"

instance (Data a, Data b) => Data (a ::-> b) where
  gfoldl _ z (Resolver _) = z (Resolver $ const undefined)
  gunfold _ z _ = z (Resolver $ const undefined)
  toConstr _ = conResolved
  dataTypeOf _ = tyResolver

conResolved :: Constr
conResolved = mkConstr tyResolver "Resolved" [] Prefix

tyResolver :: DataType
tyResolver = mkDataType "Module.Resolver" [conResolved]
