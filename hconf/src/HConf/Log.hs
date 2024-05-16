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

withColor :: Color -> String -> String
withColor c x = toColor c <> x <> toColor None

infoListEntry :: (Log m, ToString a) => a -> m ()
infoListEntry name = log $ withColor Magenta ("   - " <> toString name <> ":")

logFileChange :: (Log m) => String -> Bool -> m ()
logFileChange path changed = log ("     updated: " <> withColor (if changed then Gray else Yellow) path)

label :: (Log m) => String -> m ()
label name = info ("\n - " <> name <> ":")

info :: (Log m) => String -> m ()
info = log . withColor Green

-- infoList :: (ToString a, Log m) => String -> [a] -> m ()
-- infoList l list = info (intercalate "\n -" (l : map toString list))

warn :: (Log m) => String -> m ()
warn = log . withColor Yellow

alert :: (Log m) => String -> m ()
alert = log . withColor Red

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