{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.Data
  ( FieldD(..)
  , TypeD(..)
  , ConsD(..)
  , QueryD(..)
  , AppD(..)
  , gqlToHSWrappers
  ) where

import           Language.Haskell.TH.Lib           (appE, conE)
import           Language.Haskell.TH.Syntax        (Exp, Lift (..), Name, Q)

import           Data.Morpheus.Types.Internal.Data (DataTypeWrapper (..))

data AppD a
  = ListD (AppD a)
  | MaybeD (AppD a)
  | BaseD a
  deriving (Show)

gqlToHSWrappers :: [DataTypeWrapper] -> a -> AppD a
gqlToHSWrappers []                             = MaybeD . BaseD
gqlToHSWrappers [NonNullType]                  = BaseD
gqlToHSWrappers (NonNullType:(ListType:xs))    = ListD . gqlToHSWrappers xs
gqlToHSWrappers (NonNullType:(NonNullType:xs)) = gqlToHSWrappers xs
gqlToHSWrappers (ListType:xs)                  = MaybeD . ListD . gqlToHSWrappers xs

data QueryD = QueryD
  { queryText     :: String
  , queryTypes    :: [TypeD]
  , queryArgTypes :: [TypeD]
  } deriving (Show)

data FieldD = FieldD
  { fieldNameD :: String
  , fieldTypeD :: AppD String
  } deriving (Show)

data TypeD = TypeD
  { tName :: String
  , tCons :: [ConsD]
  } deriving (Show)

data ConsD = ConsD
  { cName   :: String
  , cFields :: [FieldD]
  } deriving (Show)

instance Lift QueryD where
  lift (QueryD n t a) = apply 'QueryD [lift n, lift t, lift a]

instance Lift a => Lift (AppD a) where
  lift (ListD x)  = apply 'ListD [lift x]
  lift (MaybeD x) = apply 'MaybeD [lift x]
  lift (BaseD x)  = apply 'BaseD [lift x]

instance Lift FieldD where
  lift (FieldD n t) = apply 'FieldD [lift n, lift t]

instance Lift TypeD where
  lift (TypeD n t) = apply 'TypeD [lift n, lift t]

instance Lift ConsD where
  lift (ConsD n t) = apply 'ConsD [lift n, lift t]

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)
