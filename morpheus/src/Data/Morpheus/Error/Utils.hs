
module Data.Morpheus.Error.Utils
    (errorMessage)
where

import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Text                      ( Text )
import           Data.Morpheus.Types.Error      ( GQLError(..)
                                                , ErrorLocation(..)
                                                , GQLErrors
                                                )


errorMessage :: Text -> GQLErrors
errorMessage x = [GQLError { message = x, locations = [ErrorLocation 0 0] }]
