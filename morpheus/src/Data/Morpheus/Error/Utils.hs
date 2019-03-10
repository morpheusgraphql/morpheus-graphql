
module Data.Morpheus.Error.Utils
    ( errorMessage
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
                                                )


errorMessage :: LineMarks -> Position -> Text -> GQLErrors
errorMessage list loc text =
    [GQLError { message = text, locations = [errorLocation loc list] }]

lineIndexAndNumber position lines =
    (length linesBefore + 1, linePos linesBefore)
  where
    linesBefore = filter (position >=) lines
    linePos [] = 1
    linePos x  = (maximum linesBefore) + 1


errorLocation pos lineMarks = do
    let (line, position) = lineIndexAndNumber pos lineMarks
    ErrorLocation line (pos - position)
