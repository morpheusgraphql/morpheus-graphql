
module Data.Morpheus.Error.Utils
    ( errorMessage
    , renderErrors
    )
where

import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..)
                                                , Position(..)
                                                , LineMarks
                                                )
import           Data.Text                      ( Text )
import           Data.Morpheus.Types.Error      ( GQLError(..)
                                                , ErrorLocation(..)
                                                , GQLErrors
                                                , JSONError(..)
                                                )


errorMessage :: Position -> Text -> GQLErrors
errorMessage pos text = [GQLError { desc = text, posIndex = pos }]

renderErrors :: LineMarks -> [GQLError] -> [JSONError]
renderErrors x = map (renderError x)

renderError :: LineMarks -> GQLError -> JSONError
renderError lineBreaks error = JSONError
    { message   = desc error
    , locations = [errorLocation lineBreaks $ posIndex error]
    }

lineIndexAndNumber position lines =
    (length linesBefore + 1, linePos linesBefore)
  where
    linesBefore = filter (position >=) lines
    linePos [] = 1
    linePos x  = (maximum linesBefore) + 1


errorLocation lineBreaks pos = do
    let (line, position) = lineIndexAndNumber pos lineBreaks
    ErrorLocation line (pos - position)
