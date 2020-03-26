{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TypeFamilies    #-}

module Server.Mythology.Place
  ( Realm(..)
  , City(..)
  )
where

import           Data.Morpheus.Types            ( GQLType(..) )
import           GHC.Generics                   ( Generic )

data Realm
  = MountOlympus
  | Sky
  | Sea
  | Underworld
  | Dream
  deriving (Generic, GQLType)

data City
  = Athens
  | Colchis
  | Delphi
  | Ithaca
  | Sparta
  | Troy
  deriving (Generic, GQLType)
