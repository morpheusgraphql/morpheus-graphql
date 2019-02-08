{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveDataTypeable #-}

module Data.GraphqlHS.Types.Error
    ( GQLError(..)
    , ErrorLocation(..)
    )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Data                      ( Data )

data ErrorLocation = ErrorLocation {
    line :: Int
    ,column :: Int
} deriving (Show , Generic, ToJSON,Data,FromJSON)


data GQLError = GQLError {
    message::Text,
    locations:: [ErrorLocation]
} deriving (Show , Generic, Data, ToJSON,FromJSON)


