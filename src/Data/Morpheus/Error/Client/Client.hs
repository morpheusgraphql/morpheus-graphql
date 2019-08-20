module Data.Morpheus.Error.Client.Client
  ( renderGQLErrors
  ) where

import           Data.Aeson                              (encode)
import           Data.ByteString.Lazy.Char8              (unpack)
import           Data.Morpheus.Error.Utils               (renderErrors)
import           Data.Morpheus.Types.Internal.Validation (GQLErrors)

renderGQLErrors :: GQLErrors -> String
renderGQLErrors = unpack . encode . renderErrors
