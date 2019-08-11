module Data.Morpheus.Client.Utils
  ( simpleName
  , capital
  , nonCapital
  , nameSpaceWith
  ) where

import           Data.Char           (toLower, toUpper)
import           Language.Haskell.TH (Name, mkName, nameBase)

simpleName :: Name -> Name
simpleName nm =
  let s = nameBase nm
   in case dropWhile (/= ':') s of
        []   -> mkName s
        _:[] -> mkName s
        _:t  -> mkName t

nameSpaceWith :: String -> String -> String
nameSpaceWith nSpace name = nonCapital nSpace <> capital name

nonCapital :: String -> String
nonCapital []     = []
nonCapital (x:xs) = toLower x : xs

capital :: String -> String
capital []     = []
capital (x:xs) = toUpper x : xs
