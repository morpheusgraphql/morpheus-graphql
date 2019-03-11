module Data.Morpheus.Error.Utils
  ( errorMessage
  , renderErrors
  ) where

import           Data.Morpheus.Types.Error    (ErrorLocation (..), GQLError (..), GQLErrors,
                                               JSONError (..))
import           Data.Morpheus.Types.MetaInfo (LineBreaks, Position)
import           Data.Text                    (Text)

errorMessage :: Position -> Text -> GQLErrors
errorMessage pos text = [GQLError {desc = text, posIndex = pos}]

renderErrors :: LineBreaks -> [GQLError] -> [JSONError]
renderErrors x = map (renderError x)

renderError :: LineBreaks -> GQLError -> JSONError
renderError lineBreaks internError =
  JSONError {message = desc internError, locations = [errorLocation lineBreaks $ posIndex internError]}

lineIndexAndNumber :: Position -> LineBreaks -> (Int, Int)
lineIndexAndNumber position lineBreaks = (length linesBefore + 1, linePos linesBefore)
  where
    linesBefore = filter (position >=) lineBreaks
    linePos [] = 1
    linePos _  = maximum linesBefore + 1

errorLocation :: LineBreaks -> Position -> ErrorLocation
errorLocation lineBreaks pos = do
  let (lineBreaks', position) = lineIndexAndNumber pos lineBreaks
  ErrorLocation lineBreaks' (pos - position)
