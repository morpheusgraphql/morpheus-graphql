{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Log
  ( label,
    task,
    warn,
    alert,
    logFileChange,
    Log (..),
    info,
    field,
  )
where

import HConf.Utils (Name)
import Relude

class Log m where
  log :: String -> m ()
  inside :: m a -> m a

newLine :: (Log m) => m ()
newLine = log ""

colored :: Color -> String -> String
colored c x = toColor c <> x <> toColor None

li :: (ToString a) => a -> String
li e = "- " <> toString e <> ":"

label :: (Log m, Monad m) => String -> m () -> m ()
label name m = newLine >> info (li name) >> inside m

task :: (Log m, Monad m) => Name -> m () -> m ()
task name m = log (colored Magenta (li name)) >> inside m

field :: (Log m) => String -> String -> m ()
field name = log . ((name <> ": ") <>)

logFileChange :: (Log m) => String -> Bool -> m ()
logFileChange path changed = field "updated" $ colored (if changed then Gray else Yellow) path

info :: (Log m) => String -> m ()
info = log . colored Green

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