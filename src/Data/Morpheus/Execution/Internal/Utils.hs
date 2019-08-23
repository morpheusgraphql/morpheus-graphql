module Data.Morpheus.Execution.Internal.Utils
  ( capital
  , nonCapital
  , nameSpaceWith
  ) where

import           Data.Char      (toLower, toUpper)
import           Data.Semigroup ((<>))

nameSpaceWith :: String -> String -> String
nameSpaceWith nSpace name = nonCapital nSpace <> capital name

nonCapital :: String -> String
nonCapital []     = []
nonCapital (x:xs) = toLower x : xs

capital :: String -> String
capital []     = []
capital (x:xs) = toUpper x : xs
