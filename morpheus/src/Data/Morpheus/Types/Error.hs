{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveDataTypeable #-}

module Data.Morpheus.Types.Error
    ( GQLError(..)
    , ErrorLocation(..)
    , GQLErrors
    )
where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

type GQLErrors = [GQLError ]

data ErrorLocation = ErrorLocation {
    line :: Int
    ,column :: Int
} deriving (Show , Generic, ToJSON,Data,FromJSON)


data GQLError = GQLError {
    message::Text,
    locations:: [ErrorLocation]
} deriving (Show , Generic, Data, ToJSON,FromJSON)


