{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TH
  ( ConsD (..),
    mkCons,
    isEnum,
    mkConsEnum,
    TypeNameTH (..),
  )
where

import Data.Morpheus.Internal.Utils (elems)
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    TypeName,
    TypeRef (..),
    hsTypeName,
  )
import Data.Morpheus.Types.Internal.AST.Fields
  ( FieldDefinition (..),
    FieldsDefinition,
  )
import Data.Morpheus.Types.Internal.AST.TypeSystem
  ( DataEnumValue (..),
  )
import Prelude
  ( (.),
    Bool (..),
    Show,
    all,
    fmap,
    null,
  )

toHSFieldDefinition :: FieldDefinition cat -> FieldDefinition cat
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

data ConsD cat = ConsD
  { cName :: TypeName,
    cFields :: [FieldDefinition cat]
  }
  deriving (Show)

mkCons :: TypeName -> FieldsDefinition cat -> ConsD cat
mkCons typename fields =
  ConsD
    { cName = hsTypeName typename,
      cFields = fmap toHSFieldDefinition (elems fields)
    }

isEnum :: [ConsD cat] -> Bool
isEnum = all (null . cFields)

mkConsEnum :: DataEnumValue -> ConsD cat
mkConsEnum DataEnumValue {enumName} = ConsD {cName = hsTypeName enumName, cFields = []}
