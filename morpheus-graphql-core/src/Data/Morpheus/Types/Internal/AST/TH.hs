{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Types.Internal.AST.TH
  ( ConsD (..),
    mkCons,
    isEnum,
    mkConsEnum,
  )
where

import Data.Morpheus.Internal.Utils (Collection, elems)
import Data.Morpheus.Types.Internal.AST.Base
  ( Description,
    FieldName,
    TypeKind,
    TypeName,
    TypeRef (..),
    VALID,
    hsTypeName,
  )
import Data.Morpheus.Types.Internal.AST.Data
  ( ANY,
    ANY,
    DataEnumValue (..),
    Directives,
    FieldContent,
    FieldDefinition (..),
    FieldsDefinition,
    IN,
    TypeDefinition,
  )

toHSFieldDefinition :: FieldDefinition cat -> FieldDefinition cat
toHSFieldDefinition field@FieldDefinition {fieldType = tyRef@TypeRef {typeConName}} =
  field
    { fieldType = tyRef {typeConName = hsTypeName typeConName}
    }

data ClientTypeDefinition = ClientTypeDefinition
  { clientTypeName :: NameTH,
    clientArgTypes :: [ClientTypeDefinition],
    clientOriginalType :: TypeDefinition ANY,
    clientCons :: [ConsD IN]
  }
  deriving (Show)

data NameTH = NameTH [FieldName] TypeName
  deriving (Show)

-- Template Haskell Types

data ConsD cat = ConsD
  { cName :: TypeName,
    cFields :: [FieldDefinition cat]
  }
  deriving (Show)

mkCons :: TypeName -> FieldsDefinition cat -> ConsD cat
mkCons typename fields =
  ConsD
    { cName = hsTypeName typename,
      cFields = map toHSFieldDefinition (elems fields)
    }

isEnum :: [ConsD cat] -> Bool
isEnum = all (null . cFields)

mkConsEnum :: DataEnumValue -> ConsD cat
mkConsEnum DataEnumValue {enumName} = ConsD {cName = hsTypeName enumName, cFields = []}
