-- Template Haskell Types
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TH
  ( ConsD (..),
  )
where

import Data.Morpheus.Types.Internal.AST.Name
  ( TypeName,
  )
import Relude

data ConsD f = ConsD
  { cName :: TypeName,
    cFields :: [f]
  }
  deriving (Show)
