{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

module Mythology.Place.Places
  ( Realm(..)
  , City(..)
  ) where

import           Data.Morpheus.Kind  (ENUM, KIND)
import           Data.Morpheus.Types (GQLType)
import           GHC.Generics        (Generic)

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
