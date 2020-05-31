module Data.Morpheus.Server.Internal.TH.Types
  ( TypeD (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    Description,
    FieldName,
    IN,
    TypeDefinition,
    TypeKind,
    TypeName,
  )

--- Core
data TypeD cat = TypeD
  { tName :: TypeName,
    tNamespace :: [FieldName],
    typeArgD :: [TypeD IN],
    tCons :: [ConsD cat],
    tKind :: TypeKind,
    tDescription :: Maybe Description,
    typeOriginal :: TypeDefinition ANY
  }
  deriving (Show)
