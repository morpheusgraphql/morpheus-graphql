module Data.Morpheus.Internal.Utils
  ( capital,
    nonCapital,
    nameSpaceField,
    nameSpaceType,
    isEnum,
    capitalTypeName,
  )
where

import Data.Char
  ( toLower,
    toUpper,
  )
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    FieldName,
    Name (..),
    Token,
    TypeName (..),
  )
import Data.Semigroup ((<>))
import qualified Data.Text as T
  ( concat,
    pack,
    unpack,
  )

mapText :: (String -> String) -> Token -> Token
mapText f = T.pack . f . T.unpack

nameSpaceType :: [Name] -> TypeName -> TypeName
nameSpaceType list (TypeName name) = TypeName . T.concat $ map capital (map readName list <> [name])

nameSpaceField :: TypeName -> FieldName -> FieldName
nameSpaceField nSpace (Name name) = Name (nonCapital nSpace <> capital name)

nonCapital :: TypeName -> Token
nonCapital = mapText __nonCapital . readTypeName
  where
    __nonCapital [] = []
    __nonCapital (x : xs) = toLower x : xs

capital :: Token -> Token
capital = mapText __capital
  where
    __capital [] = []
    __capital (x : xs) = toUpper x : xs

isEnum :: [ConsD] -> Bool
isEnum = all (null . cFields)

capitalTypeName :: FieldName -> TypeName
capitalTypeName = TypeName . capital . readName
