{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Utils
  ( errorMessage,
    globalErrorMessage,
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( Message,
    Position (..),
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( GQLError (..),
    GQLErrors,
  )
import Relude hiding (ByteString)

errorMessage :: Position -> Message -> GQLErrors
errorMessage position message =
  [ GQLError
      { message,
        locations = [position],
        extensions = Nothing
      }
  ]

globalErrorMessage :: Message -> GQLErrors
globalErrorMessage message =
  [ GQLError
      { message,
        locations = [],
        extensions = Nothing
      }
  ]
