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
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    pack,
    unpack,
  )
import qualified Data.Text as T
  ( concat,
  )

nameSpaceType :: [Text] -> Text -> Text
nameSpaceType list name = T.concat $ map capital (list <> [name])

nameSpaceWith :: Text -> Text -> Text
nameSpaceWith nSpace name = nonCapital nSpace <> capital name

nonCapital :: Text -> Text
nonCapital = pack . __nonCapital . unpack
  where
    __nonCapital [] = []
    __nonCapital (x : xs) = toLower x : xs

capital :: Text -> Text
capital = pack . __capital . unpack
  where
    __capital [] = []
    __capital (x : xs) = toUpper x : xs

isEnum :: [ConsD] -> Bool
isEnum = all (null . cFields)
