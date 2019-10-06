module Data.Morpheus.Execution.Internal.Utils
  ( capital
  , nonCapital
  , nameSpaceWith
  , nameSpaceType
  ) where

import           Data.Char      (toLower, toUpper)
import           Data.Semigroup ((<>))
import           Data.Text      (Text, unpack)

nameSpaceType :: [Text] -> Text -> String
nameSpaceType [] name     = capital (unpack name)
nameSpaceType (x:xs) name = capital (unpack name) <> nameSpaceType xs name

nameSpaceWith :: String -> String -> String
nameSpaceWith nSpace name = nonCapital nSpace <> capital name

nonCapital :: String -> String
nonCapital []     = []
nonCapital (x:xs) = toLower x : xs

capital :: String -> String
capital []     = []
capital (x:xs) = toUpper x : xs
