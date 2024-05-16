{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Log
  ( label,
    infoListEntry,
    warn,
    alert,
    logFileChange,
    Log (..),
    info,
  )
where

import Relude

class Log m where
  log :: String -> m ()

indent :: Int -> String
indent i = replicate (i) ' '

colored :: Color -> String -> String
colored c x = toColor c <> x <> toColor None

li :: (ToString a) => a -> String
li e = "- " <> toString e <> ":"

label :: (Log m) => String -> m ()
label name = info ("\n" <> indent 1 <> li name)

infoListEntry :: (Log m, ToString a) => a -> m ()
infoListEntry name = log $ colored Magenta (indent 3 <> li name)

logFileChange :: (Log m) => String -> Bool -> m ()
logFileChange path changed = log (indent 5 <> "updated: " <> colored (if changed then Gray else Yellow) path)

info :: (Log m) => String -> m ()
info = log . colored Green

-- infoList :: (ToString a, Log m) => String -> [a] -> m ()
-- infoList l list = info (intercalate "\n -" (l : map toString list))

warn :: (Log m) => String -> m ()
warn = log . colored Yellow

alert :: (Log m) => String -> m ()
alert = log . colored Red

data Color
  = Red
  | Green
  | Yellow
  | Gray
  | Magenta
  | None

toColor :: Color -> String
toColor c = "\x1b[" <> show (colorCode c) <> "m"

colorCode :: Color -> Int
colorCode Red = 31
colorCode Green = 32
colorCode Yellow = 33
colorCode Gray = 90
colorCode Magenta = 95
colorCode None = 0