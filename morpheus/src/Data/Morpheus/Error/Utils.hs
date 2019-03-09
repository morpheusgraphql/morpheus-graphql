
module Data.Morpheus.Error.Utils
    ( errorMessage
    , Errors
    )
where

import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Text                      ( Text )
import           Data.Morpheus.Types.Error      ( GQLError(..)
                                                , ErrorLocation(..)
                                                )

type Errors = [ [Int] ->  GQLError ]


errorMessage :: Text -> Errors
errorMessage x =
    [\_ -> GQLError { message = x, locations = [ErrorLocation 0 0] }]
