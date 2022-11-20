{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Server.Deriving.Utils.AST
  ( argumentsToObject,
  )
where

import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    ObjectEntry (..),
    VALID,
    Value (..),
  )

argumentsToObject :: Arguments VALID -> Value VALID
argumentsToObject = Object . fmap toEntry
  where
    toEntry Argument {..} = ObjectEntry argumentName argumentValue
