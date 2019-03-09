{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveDataTypeable #-}

module Data.Morpheus.Types.Error
    ( GQLError(..)
    , ErrorLocation(..)
    , Position(..)
    )
where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype Position = Position { getPos :: Int } deriving (Show , Generic)

data ErrorLocation = ErrorLocation {
    line :: Int
    ,column :: Int
} deriving (Show , Generic, ToJSON,Data,FromJSON)


data GQLError = GQLError {
    message::Text,
    locations:: [ErrorLocation]
} deriving (Show , Generic, Data, ToJSON,FromJSON)


