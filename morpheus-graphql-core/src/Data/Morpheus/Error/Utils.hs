{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Morpheus.Error.Utils
  ( errorMessage
  , globalErrorMessage
  , badRequestError
  )
where

import           Data.Semigroup                         ((<>))
import           Data.ByteString.Lazy.Char8     ( ByteString
                                                , pack
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position(..)
                                                , GQLError(..)
                                                , GQLErrors
                                                )
import           Data.Text                      ( Text )


errorMessage :: Position -> Text -> GQLErrors
errorMessage position message = [GQLError { message, locations = [position] }]

globalErrorMessage :: Text -> GQLErrors
globalErrorMessage message = [GQLError { message, locations = [] }]

badRequestError :: String -> ByteString
badRequestError = ("Bad Request. Could not decode Request body: " <>) . pack 