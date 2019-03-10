
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


errorMessage :: LineMarks -> Position -> Text -> GQLErrors
errorMessage list loc text = [GQLError { desc = text, posIndex = loc }]

renderErrors :: [GQLError] -> [JSONError]
renderErrors = map renderError

renderError :: GQLError -> JSONError
renderError error = JSONError
    { message   = desc error
    , locations = [errorLocation [] $ posIndex error]
    }

lineIndexAndNumber position lines =
    (length linesBefore + 1, linePos linesBefore)
  where
    linesBefore = filter (position >=) lines
    linePos [] = 1
    linePos x  = (maximum linesBefore) + 1


errorLocation lineMarks pos = do
    let (line, position) = lineIndexAndNumber pos lineMarks
    ErrorLocation line (pos - position)
