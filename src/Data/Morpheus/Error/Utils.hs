{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Morpheus.Error.Utils
  ( errorMessage
  , globalErrorMessage
  , badRequestError
  , toLocation
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
import           Text.Megaparsec                ( SourcePos(SourcePos)
                                                , sourceColumn
                                                , sourceLine
                                                , unPos
                                                )

errorMessage :: Position -> Text -> GQLErrors
errorMessage position message = [GQLError { message, locations = [position] }]

globalErrorMessage :: Text -> GQLErrors
globalErrorMessage message = [GQLError { message, locations = [] }]

toLocation :: SourcePos -> Position
toLocation SourcePos { sourceLine, sourceColumn } =
  Position { line = unPos sourceLine, column = unPos sourceColumn }

badRequestError :: String -> ByteString
badRequestError = ("Bad Request. Could not decode Request body: " <>) . pack 