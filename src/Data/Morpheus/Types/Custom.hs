{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.Morpheus.Types.Custom where

import           Data.Morpheus.Kind          (KIND, OBJECT)
import           Data.Morpheus.Types.GQLType (GQLType)
import           GHC.Generics                (Generic)

type instance KIND (Tuple a b) = OBJECT

data Tuple a b = Tuple
  { key   :: a
  , value :: b
  } deriving (Generic, GQLType)
