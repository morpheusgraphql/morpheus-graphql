{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error
  ( errorMessage,
    globalErrorMessage,
    gqlWarnings,
    renderGQLErrors,
    deprecatedField,
    subfieldsNotSelected,
    NameCollision (..),
  )
where

import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Error.Selection
  ( subfieldsNotSelected,
  )
import Data.Morpheus.Error.Utils
import Data.Morpheus.Error.Warning
  ( deprecatedField,
    gqlWarnings,
    renderGQLErrors,
  )
