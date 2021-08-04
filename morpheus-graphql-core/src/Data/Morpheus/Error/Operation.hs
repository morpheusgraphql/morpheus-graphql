{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Operation
  ( mutationIsNotDefined,
    subscriptionIsNotDefined,
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( Position,
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( GQLError,
    at,
  )

mutationIsNotDefined :: Position -> GQLError
mutationIsNotDefined position =
  "Schema is not configured for mutations." `at` position

subscriptionIsNotDefined :: Position -> GQLError
subscriptionIsNotDefined position =
  "Schema is not configured for subscriptions." `at` position
