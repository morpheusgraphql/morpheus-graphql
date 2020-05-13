module Data.Morpheus.Internal.Utils
  ( capital,
    nonCapital,
    nameSpaceWith,
    nameSpaceType,
    isEnum,
  )
where

import Data.Char
  ( toLower,
    toUpper,
  )
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    Name,
  )
import Data.Semigroup ((<>))
import qualified Data.Text as T
  ( concat,
    pack,
    unpack,
  )

mapName :: (String -> String) -> Name -> Name
mapName f = T.pack . f . T.unpack

nameSpaceType :: [Name] -> Name -> Name
nameSpaceType list name = T.concat $ map capital (list <> [name])

nameSpaceWith :: Name -> Name -> Name
nameSpaceWith nSpace name = nonCapital nSpace <> capital name

nonCapital :: Name -> Name
nonCapital = mapName __nonCapital
  where
    __nonCapital [] = []
    __nonCapital (x : xs) = toLower x : xs

capital :: Name -> Name
capital = mapName __capital
  where
    __capital [] = []
    __capital (x : xs) = toUpper x : xs

isEnum :: [ConsD] -> Bool
isEnum = all (null . cFields)
