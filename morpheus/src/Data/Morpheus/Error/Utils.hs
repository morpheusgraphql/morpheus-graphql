
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

lineIndexAndNumber position lines = (length linesBefore, linePos linesBefore)
  where
    linesBefore = filter (position >=) lines
    linePos [] = 1
    linePos x  = (maximum linesBefore) + 1


errorLocation (Position loc) lineMarks = do
    let (line, position) = lineIndexAndNumber loc lineMarks
    ErrorLocation line (loc - position)
