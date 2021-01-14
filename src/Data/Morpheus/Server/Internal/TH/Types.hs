{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
    ServerDec,
    ServerDecContext (..),
    ServerConsD,
    ServerFieldDefinition,
  )
where

import Control.Monad.Reader (Reader)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    FieldDefinition,
    IN,
    TypeDefinition,
    TypeKind,
    TypeName,
  )
import Prelude
  ( Bool,
    Maybe,
    Show,
  )

type ServerFieldDefinition cat s = (Bool, FieldDefinition cat s)

type ServerConsD cat s = ConsD (ServerFieldDefinition cat s)

--- Core
data ServerTypeDefinition cat s = ServerTypeDefinition
  { tName :: TypeName,
    typeArgD :: [ServerTypeDefinition IN s],
    tCons :: [ServerConsD cat s],
    tKind :: TypeKind,
    typeOriginal :: Maybe (TypeDefinition ANY s)
  }
  deriving (Show)

type ServerDec = Reader ServerDecContext

newtype ServerDecContext = ServerDecContext
  { namespace :: Bool
  }
