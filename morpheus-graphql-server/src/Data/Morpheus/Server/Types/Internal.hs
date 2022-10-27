{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.Internal
  ( TypeData (..),
    mkTypeData,
    stripConstructorNamespace,
    stripFieldNamespace,
  )
where

-- MORPHEUS

import Data.Char (toLower)
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint (..))
import Data.Morpheus.Types.Internal.AST
  ( TypeName,
    TypeWrapper (..),
    mkBaseType,
  )
import Data.Text (length)
import Relude hiding (Seq, Undefined, intercalate, length)

data TypeData = TypeData
  { gqlTypeName :: TypeName,
    gqlWrappers :: TypeWrapper,
    gqlFingerprint :: TypeFingerprint
  }
  deriving (Show)

mkTypeData :: TypeName -> a -> TypeData
mkTypeData name _ =
  TypeData
    { gqlTypeName = name,
      gqlFingerprint = InternalFingerprint name,
      gqlWrappers = mkBaseType
    }

dropPrefix :: Text -> String -> String
dropPrefix name = drop (length name)

stripConstructorNamespace :: Text -> String -> String
stripConstructorNamespace = dropPrefix

stripFieldNamespace :: Text -> String -> String
stripFieldNamespace prefix = __uncapitalize . dropPrefix prefix
  where
    __uncapitalize [] = []
    __uncapitalize (x : xs) = toLower x : xs
