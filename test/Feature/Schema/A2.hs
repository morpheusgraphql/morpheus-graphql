{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

module Feature.Schema.A2
  ( A(..)
  ) where

import           Data.Morpheus.Kind  (KIND, OBJECT)
import           Data.Morpheus.Types (GQLType (..))
import           GHC.Generics        (Generic)

type instance KIND A = OBJECT

newtype A = A
  { bla :: Int
  } deriving (Generic, GQLType)
