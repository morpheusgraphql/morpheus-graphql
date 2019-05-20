{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.Morpheus.Types.ID
  ( ID(..)
  ) where

import           Data.Morpheus.Kind.GQLType  (GQLType)
import           Data.Morpheus.Kind.Internal (KIND, SCALAR)
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)

type instance KIND ID = SCALAR

newtype ID = ID
  { unpackID :: Text
  } deriving (Generic, GQLType)
