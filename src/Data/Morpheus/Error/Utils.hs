{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Error.Utils
  ( errorMessage
  , globalErrorMessage
  , renderErrors
  , renderError
  , badRequestError
  , toLocation
  )
where

import           Data.ByteString.Lazy.Char8     ( ByteString
                                                , pack
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position )
import           Data.Morpheus.Types.Internal.Validation
                                                ( GQLError(..)
                                                , GQLErrors
                                                , JSONError(..)
                                                , Position(..)
                                                )
import           Data.Text                      ( Text )
import           Text.Megaparsec                ( SourcePos(SourcePos)
                                                , sourceColumn
                                                , sourceLine
                                                , unPos
                                                )

errorMessage :: Position -> Text -> GQLErrors
errorMessage position desc = [GQLError { desc, positions = [position] }]

globalErrorMessage :: Text -> GQLErrors
globalErrorMessage desc = [GQLError { desc, positions = [] }]

renderErrors :: [GQLError] -> [JSONError]
renderErrors = map renderError

renderError :: GQLError -> JSONError
renderError GQLError { desc, positions } =
  JSONError { message = desc, locations = positions }

toLocation :: SourcePos -> Position
toLocation SourcePos { sourceLine, sourceColumn } =
  Position { line = unPos sourceLine, column = unPos sourceColumn }

badRequestError :: String -> ByteString
badRequestError aesonError' =
  pack $ "Bad Request. Could not decode Request body: " ++ aesonError'
