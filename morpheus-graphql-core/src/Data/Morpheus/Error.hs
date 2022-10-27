{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error
  ( renderGQLErrors,
    deprecatedField,
    subfieldsNotSelected,
    NameCollision (..),
    gqlWarnings,
    printWarning,
    printError,
  )
where

import Data.Mergeable
import Data.Morpheus.Error.Selection
import Data.Morpheus.Error.Warning
