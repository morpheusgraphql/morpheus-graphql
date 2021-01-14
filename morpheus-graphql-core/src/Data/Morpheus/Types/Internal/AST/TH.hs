{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TH
  ( ConsD (..),
    mkCons,
    mkConsEnum,
    TypeNameTH (..),
  )
where

import Data.Morpheus.Internal.Utils (elems)
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    TypeName,
    hsTypeName,
  )
import Data.Morpheus.Types.Internal.AST.Fields
  ( FieldDefinition (..),
    FieldsDefinition,
  )
import Data.Morpheus.Types.Internal.AST.Type (TypeRef (..))
import Data.Morpheus.Types.Internal.AST.TypeSystem
  ( DataEnumValue (..),
  )
import Relude

toHSFieldDefinition :: FieldDefinition cat s -> FieldDefinition cat s
toHSFieldDefinition field@FieldDefinition {fieldType = tyRef@TypeRef {typeConName}} =
  field
    { fieldType = tyRef {typeConName = hsTypeName typeConName}
    }

data TypeNameTH = TypeNameTH
  { namespace :: [FieldName],
    typename :: TypeName
  }
  deriving (Show)

-- Template Haskell Types

data ConsD cat s = ConsD
  { cName :: TypeName,
    cFields :: [FieldDefinition cat s]
  }
  deriving (Show)

mkCons :: TypeName -> FieldsDefinition cat s -> ConsD cat s
mkCons typename fields =
  ConsD
    { cName = hsTypeName typename,
      cFields = fmap toHSFieldDefinition (elems fields)
    }

mkConsEnum :: DataEnumValue s -> ConsD cat s
mkConsEnum DataEnumValue {enumName} = ConsD {cName = hsTypeName enumName, cFields = []}
