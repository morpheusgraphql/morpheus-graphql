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
  ( GQLError,
    at,
    msg,
  )

{-# DEPRECATED errorMessage "\"my error\" `at` position" #-}
errorMessage :: Position -> Message -> [GQLError]
errorMessage position message = [msg message `at` position]

{-# DEPRECATED globalErrorMessage "use validation errors" #-}
globalErrorMessage :: Message -> [GQLError]
globalErrorMessage message = [msg message]
