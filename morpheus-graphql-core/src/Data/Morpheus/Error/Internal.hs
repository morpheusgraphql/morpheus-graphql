{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Internal
  ( internalError,
  )
where

-- MORPHEUS
import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLErrors,
    Message,
  )
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Failure (..),
  )
import Data.Semigroup ((<>))
import Prelude (($))

-- all kind internal error in development
internalError :: Failure GQLErrors m => Message -> m a
internalError x = failure $ globalErrorMessage $ "INTERNAL ERROR: " <> x
