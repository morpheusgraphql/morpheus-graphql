{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Types.Selections where

import           Control.Monad.Trans.Except (ExceptT (..))
import           Data.Data
import           Data.Morpheus.Types.Error  (GQLErrors)
import           GHC.Generics               (Generic)

type ResolveIO = ExceptT GQLErrors IO

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
