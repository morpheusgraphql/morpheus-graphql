{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

module Example.Places.Places
  ( Places
  ) where

import           Data.Morpheus.Kind (ENUM, GQLType, KIND)
import           GHC.Generics       (Generic)

type instance KIND Places = ENUM

data Places
  = Olympus
  | Athens
  | Colchis
  | Underworld
  | Fantasy
  deriving (Generic, GQLType)
