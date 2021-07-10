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
  ( ValidationError,
    at,
  )

mutationIsNotDefined :: Position -> ValidationError
mutationIsNotDefined position =
  "Schema is not configured for mutations." `at` position

subscriptionIsNotDefined :: Position -> ValidationError
subscriptionIsNotDefined position =
  "Schema is not configured for subscriptions." `at` position
