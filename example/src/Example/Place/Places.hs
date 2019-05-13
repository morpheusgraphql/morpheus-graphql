{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

module Example.Place.Places
  ( Places(..)
  ) where

import           Data.Morpheus.Kind (ENUM, GQLType, KIND)
import           GHC.Generics       (Generic)

type instance KIND Realms = ENUM

data Realms
  = MountOlympus
  | Sky
  | Underworld
  | Fantasy
  deriving (Generic, GQLType)

type instance KIND Cities = ENUM

data Cities
  = Athens
  | Colchis
  | Delphi
  | Ithaca
  | Sparta
  | Troy
