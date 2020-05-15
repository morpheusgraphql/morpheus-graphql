{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Types.Internal.AST.TH
  ( TypeD (..),
    ConsD (..),
    mkCons,
    GQLTypeD (..),
    isEnum,
    mkConsEnum,
  )
where

import Data.Morpheus.Internal.Utils (elems)
import Data.Morpheus.Types.Internal.AST.Base
  ( DataTypeKind,
    FieldName,
    TypeName,
    TypeRef (..),
    hsTypeName,
  )
import Data.Morpheus.Types.Internal.AST.Data
  ( ANY,
    DataEnumValue (..),
    FieldDefinition (..),
    FieldsDefinition,
    Meta,
    TypeDefinition,
  )

toHSFieldDefinition :: FieldDefinition cat -> FieldDefinition cat
toHSFieldDefinition field@FieldDefinition {fieldType = tyRef@TypeRef {typeConName}} =
  field
    { fieldType = tyRef {typeConName = hsTypeName typeConName}
    }

-- Template Haskell Types
-- Document
data GQLTypeD = GQLTypeD
  { typeD :: TypeD,
    typeArgD :: [TypeD],
    typeOriginal :: TypeDefinition ANY
  }
  deriving (Show)

--- Core
data TypeD = TypeD
  { tName :: TypeName,
    tNamespace :: [FieldName],
    tCons :: [ConsD],
    tKind :: DataTypeKind,
    tMeta :: Maybe Meta
  }
  deriving (Show)

data ConsD = ConsD
  { cName :: TypeName,
    cFields :: [FieldDefinition ANY]
  }
  deriving (Show)

mkCons :: TypeName -> FieldsDefinition cat -> ConsD
mkCons typename fields =
  ConsD
    { cName = hsTypeName typename,
      cFields = map (toHSFieldDefinition . mockFieldDefinition) (elems fields)
    }

isEnum :: [ConsD] -> Bool
isEnum = all (null . cFields)

mockFieldDefinition :: FieldDefinition a -> FieldDefinition b
mockFieldDefinition FieldDefinition {..} = FieldDefinition {..}

mkConsEnum :: DataEnumValue -> ConsD
mkConsEnum DataEnumValue {enumName} = ConsD {cName = hsTypeName enumName, cFields = []}
