{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Example.Character.Deity
  ( Deity(..)
  ) where

import           Data.Morpheus.Kind   (GQLType (..), KIND, OBJECT)
import           Data.Text            (Text)
import           Example.Place.Places (Places)
import           GHC.Generics         (Generic)

type instance KIND Deity = OBJECT

instance GQLType Deity where
  description _ = "Custom Description for Client Defined User Type"

data Deity = Deity
  { fullName :: Text -- Non-Nullable Field
  , power    :: Maybe Text -- Nullable Field
  , realm    :: Places
  } deriving (Generic)
