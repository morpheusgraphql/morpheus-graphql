module Data.Morpheus.Execution.Internal.Utils
  ( capital
  , nonCapital
  , nameSpaceWith
  , nameSpaceType
  , nameSpaceTypeString
  )
where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , unpack
                                                )


nameSpaceTypeString :: [String] -> String -> String
nameSpaceTypeString list name = concatMap capital (list <> [name])

nameSpaceType :: [Text] -> Text -> String
nameSpaceType list name = concatMap (capital . unpack) (list <> [name])

nameSpaceWith :: String -> String -> String
nameSpaceWith nSpace name = nonCapital nSpace <> capital name

nonCapital :: String -> String
nonCapital []       = []
nonCapital (x : xs) = toLower x : xs

capital :: String -> String
capital []       = []
capital (x : xs) = toUpper x : xs
