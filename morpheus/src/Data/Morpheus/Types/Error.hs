{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Data.Morpheus.Types.Error
    ( GQLError(..)
    , ErrorLocation(..)
    , GQLErrors
    , InternalError
    )
where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


data InternalError = InternalError {
    desc :: Text ,
    posIndex :: Int
} deriving (Show)

type GQLErrors = [GQLError ]

data ErrorLocation = ErrorLocation {
    line :: Int
    ,column :: Int
} deriving (Show , Generic, ToJSON)


data GQLError = GQLError {
    message::Text,
    locations:: [ErrorLocation]
} deriving (Show , Generic , ToJSON)


