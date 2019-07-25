{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Feature.Schema.A2
  ( A(..)
  ) where

import           Data.Morpheus.Kind  (OBJECT)
import           Data.Morpheus.Types (GQLType (..))
import           GHC.Generics        (Generic)

newtype A = A
  { bla :: Int
  } deriving (Generic)

instance GQLType A where
  type KIND A = OBJECT
