module Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    FieldName,
    IN,
    TypeDefinition,
    TypeKind,
    TypeName,
  )

--- Core
data ServerTypeDefinition cat = ServerTypeDefinition
  { tName :: TypeName,
    tNamespace :: [FieldName],
    typeArgD :: [ServerTypeDefinition IN],
    tCons :: [ConsD cat],
    tKind :: TypeKind,
    typeOriginal :: Maybe (TypeDefinition ANY)
  }
  deriving (Show)
