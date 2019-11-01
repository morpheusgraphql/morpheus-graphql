{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Server.Mythology.Character.Human
  ( Human(..)
  ) where

import           Data.Morpheus.Kind     (OBJECT)
import           Data.Morpheus.Types    (GQLType (..))
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Server.Mythology.Place.Places (City (..))

data Human = Human
  { name :: Text
  , home :: City
  } deriving (Generic)

instance GQLType Human where
  type KIND Human = OBJECT
