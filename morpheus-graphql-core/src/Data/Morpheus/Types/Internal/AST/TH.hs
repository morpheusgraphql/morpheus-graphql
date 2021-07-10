-- Template Haskell Types
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TH
  ( ConsD (..),
    mkConsEnum,
  )
where

import Data.Morpheus.Types.Internal.AST.Name
  ( TypeName,
  )
import Data.Morpheus.Types.Internal.AST.TypeSystem
  ( DataEnumValue (..),
  )
import Relude

data ConsD f = ConsD
  { cName :: TypeName,
    cFields :: [f]
  }
  deriving (Show)

mkConsEnum :: DataEnumValue s -> ConsD f
mkConsEnum DataEnumValue {enumName} = ConsD {cName = enumName, cFields = []}
