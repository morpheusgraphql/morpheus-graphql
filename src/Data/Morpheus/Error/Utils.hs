{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Error.Utils
  ( errorMessage
  , globalErrorMessage
  , renderErrors
  , badRequestError
  ) where

import           Data.ByteString.Lazy.Char8              (ByteString, pack)
import           Data.Morpheus.Types.Internal.Base       (Position)
import           Data.Morpheus.Types.Internal.Validation (ErrorLocation (..), GQLError (..), GQLErrors, JSONError (..))
import           Data.Text                               (Text)
import           Text.Megaparsec                         (SourcePos (SourcePos), sourceColumn, sourceLine, unPos)

errorMessage :: Position -> Text -> GQLErrors
errorMessage position text = [GQLError {desc = text, positions = [position]}]

globalErrorMessage :: Text -> GQLErrors
globalErrorMessage text = [GQLError {desc = text, positions = []}]

renderErrors :: [GQLError] -> [JSONError]
renderErrors = map renderError

renderError :: GQLError -> JSONError
renderError GQLError {desc, positions} = JSONError {message = desc, locations = map toErrorLocation positions}

toErrorLocation :: Position -> ErrorLocation
toErrorLocation SourcePos {sourceLine, sourceColumn} =
  ErrorLocation {line = unPos sourceLine, column = unPos sourceColumn}

badRequestError :: String -> ByteString
badRequestError aesonError' = pack $ "Bad Request. Could not decode Request body: " ++ aesonError'
