{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Operation
  ( mutationIsNotDefined,
    subscriptionIsNotDefined,
  )
where

import Data.Morpheus.Error.Utils (errorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLErrors,
    Position,
  )

mutationIsNotDefined :: Position -> GQLErrors
mutationIsNotDefined position =
  errorMessage position "Schema is not configured for mutations."

subscriptionIsNotDefined :: Position -> GQLErrors
subscriptionIsNotDefined position =
  errorMessage position "Schema is not configured for subscriptions."
