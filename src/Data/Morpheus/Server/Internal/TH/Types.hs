module Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
    ServerDec,
    ServerDecContext (..),
  )
where

import Control.Monad.Reader (Reader)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    IN,
    TypeDefinition,
    TypeKind,
    TypeName,
  )

--- Core
data ServerTypeDefinition cat s = ServerTypeDefinition
  { tName :: TypeName,
    typeArgD :: [ServerTypeDefinition IN s],
    tCons :: [ConsD cat s],
    tKind :: TypeKind,
    typeOriginal :: Maybe (TypeDefinition ANY s)
  }
  deriving (Show)

type ServerDec = Reader ServerDecContext

newtype ServerDecContext = ServerDecContext
  { namespace :: Bool
  }
