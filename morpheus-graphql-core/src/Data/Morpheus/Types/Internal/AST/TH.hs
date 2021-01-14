{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TH
  ( ConsD (..),
    mkConsEnum,
    TypeNameTH (..),
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    TypeName,
    hsTypeName,
  )
import Data.Morpheus.Types.Internal.AST.TypeSystem
  ( DataEnumValue (..),
  )
import Relude

data TypeNameTH = TypeNameTH
  { namespace :: [FieldName],
    typename :: TypeName
  }
  deriving (Show)

-- Template Haskell Types
data ConsD f = ConsD
  { cName :: TypeName,
    cFields :: [f]
  }
  deriving (Show)

mkConsEnum :: DataEnumValue s -> ConsD f
mkConsEnum DataEnumValue {enumName} = ConsD {cName = hsTypeName enumName, cFields = []}
