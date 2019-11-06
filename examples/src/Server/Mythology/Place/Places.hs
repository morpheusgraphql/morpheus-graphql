{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

module Server.Mythology.Place.Places
  ( Realm(..)
  , City(..)
  )
where

import           Data.Morpheus.Kind             ( ENUM )
import           Data.Morpheus.Types            ( GQLType(..) )
import           GHC.Generics                   ( Generic )

data Realm
  = MountOlympus
  | Sky
  | Sea
  | Underworld
  | Dream
  deriving (Generic)

instance GQLType Realm where
  type KIND Realm = ENUM

data City
  = Athens
  | Colchis
  | Delphi
  | Ithaca
  | Sparta
  | Troy

instance GQLType City where
  type KIND City = ENUM
