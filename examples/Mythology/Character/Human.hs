{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Mythology.Character.Human
  ( Human(..)
  ) where

import           Data.Morpheus.Kind     (KIND, OBJECT)
import           Data.Morpheus.Types    (GQLType)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Mythology.Place.Places (City (..))

type instance KIND Human = OBJECT

data Human = Human
  { name :: Text
  , home :: City
  } deriving (Generic, GQLType)
