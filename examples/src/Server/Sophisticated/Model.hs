{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Sophisticated.Model
  ( jsonAddress
  , jsonUser
  , JSONUser(..)
  , JSONAddress(..)
  ) where

import           Data.Aeson   (FromJSON)
import           Data.Text    (Text)
import           Files        (getJson)
import           GHC.Generics (Generic)

data JSONUser = JSONUser
  { name  :: Text
  , email :: Text
  } deriving (Show, Generic, FromJSON)

data JSONAddress = JSONAddress
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  } deriving (Generic, Show, FromJSON)

jsonUser :: IO (Either String JSONUser)
jsonUser = getJson "deprecated/user"

jsonAddress :: IO (Either String JSONAddress)
jsonAddress = getJson "deprecated/address"
