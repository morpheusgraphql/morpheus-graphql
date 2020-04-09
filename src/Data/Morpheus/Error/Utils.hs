{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Morpheus.Error.Utils
  ( errorMessage
  , globalErrorMessage
  , badRequestError
  , toLocation
  , duplicateKeyError
  )
where

import           Data.Semigroup                         ((<>))
import           Data.ByteString.Lazy.Char8     ( ByteString
                                                , pack
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position(..)
                                                , Named
                                                , GQLError(..)
                                                , GQLErrors
                                                )
import           Data.Text                      ( Text )
import           Text.Megaparsec                ( SourcePos(SourcePos)
                                                , sourceColumn
                                                , sourceLine
                                                , unPos
                                                )

-- TODO: add Location
duplicateKeyError :: Named a -> GQLError
duplicateKeyError (name,_) = GQLError { message = "duplicate key \"" <> name <> "\"", locations = []}

errorMessage :: Position -> Text -> GQLErrors
errorMessage position message = [GQLError { message, locations = [position] }]

globalErrorMessage :: Text -> GQLErrors
globalErrorMessage message = [GQLError { message, locations = [] }]

toLocation :: SourcePos -> Position
toLocation SourcePos { sourceLine, sourceColumn } =
  Position { line = unPos sourceLine, column = unPos sourceColumn }

badRequestError :: String -> ByteString
badRequestError aesonError' =
  pack $ "Bad Request. Could not decode Request body: " ++ aesonError'
