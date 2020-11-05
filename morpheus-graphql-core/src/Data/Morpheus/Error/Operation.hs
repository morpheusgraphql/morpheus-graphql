{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Operation
  ( mutationIsNotDefined,
    subscriptionIsNotDefined,
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( Position,
    ValidationError (..),
  )

mutationIsNotDefined :: Position -> ValidationError
mutationIsNotDefined position =
  ValidationError
    "Schema is not configured for mutations."
    [position]

subscriptionIsNotDefined :: Position -> ValidationError
subscriptionIsNotDefined position =
  ValidationError
    "Schema is not configured for subscriptions."
    [position]
