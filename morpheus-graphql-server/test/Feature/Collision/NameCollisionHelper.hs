{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Collision.NameCollisionHelper
  ( A (..),
  )
where

import Data.Morpheus.Server.Types (GQLType)
import GHC.Generics (Generic)

newtype A = A
  { bla :: Int
  }
  deriving (Generic, GQLType)
