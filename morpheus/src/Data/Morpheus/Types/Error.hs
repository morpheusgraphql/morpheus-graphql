{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Data.Morpheus.Types.Error
    ( GQLError(..)
    , ErrorLocation(..)
    , GQLErrors
    , RenderError(..)
    )
where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


data GQLError = GQLError {
    desc :: Text ,
    posIndex :: Int
} deriving (Show)

type GQLErrors = [ GQLError ]


data ErrorLocation = ErrorLocation {
    line :: Int
    ,column :: Int
} deriving (Show , Generic, ToJSON)


data RenderError = RenderError {
    message::Text,
    locations:: [ErrorLocation]
} deriving (Show , Generic , ToJSON)


