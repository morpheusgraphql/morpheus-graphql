{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.Data
  ( FieldD(..)
  , TypeD(..)
  , ConsD(..)
  , QueryD(..)
  ) where

import           Language.Haskell.TH.Lib    (appE, conE)
import           Language.Haskell.TH.Syntax (Exp, Lift (..), Name, Q)

data QueryD = QueryD
  { queryText     :: String
  , queryTypes    :: [TypeD]
  , queryArgTypes :: [TypeD]
  } deriving (Show)

data FieldD = FieldD
  { fieldNameD :: String
  , fieldTypeD :: ([String], String)
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

instance Lift FieldD where
  lift (FieldD n t) = apply 'FieldD [lift n, lift t]

instance Lift TypeD where
  lift (TypeD n t) = apply 'TypeD [lift n, lift t]

instance Lift ConsD where
  lift (ConsD n t) = apply 'ConsD [lift n, lift t]

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)
