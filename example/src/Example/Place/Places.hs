{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

module Example.Place.Places
  ( Realm(..)
  , City(..)
  ) where

import           Data.Morpheus.Kind (ENUM, GQLType, KIND)
import           GHC.Generics       (Generic)

type instance KIND Realm = ENUM

data Realm
  = MountOlympus
  | Sky
  | Sea
  | Underworld
  | Dream
  deriving (Generic, GQLType)

type instance KIND City = ENUM

data City
  = Athens
  | Colchis
  | Delphi
  | Ithaca
  | Sparta
  | Troy
