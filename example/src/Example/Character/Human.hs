{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Example.Character.Human
  ( Human(..)
  ) where

import           Data.Morpheus.Kind   (GQLType, KIND, OBJECT)
import           Data.Text            (Text)
import           Example.Place.Places (Places)
import           GHC.Generics         (Generic)

type instance KIND Human = OBJECT

data Human = Human
  { name  :: Text
  , home :: Places
  } deriving (Generic, GQLType)
