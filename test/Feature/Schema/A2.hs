{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Schema.A2
  ( A (..),
  )
where

import Data.Morpheus.Types (GQLType)
import GHC.Generics (Generic)

newtype A = A
  { bla :: Int
  }
  deriving (Generic, GQLType)
