{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Log where

import Relude

class Log m where
  log :: String -> m ()

withColor :: String -> String -> String
withColor color x = color <> x <> noneColor

infoListEntry :: (Log m, ToString a) => a -> m ()
infoListEntry name = log $ withColor magenta (" - " <> toString name <> ":")

label :: (Log m) => String -> m ()
label name = info (name <> ":")

info :: (Log m) => String -> m ()
info = log . withColor successColor

infoList :: (ToString a, Log m) => String -> [a] -> m ()
infoList l list = info (intercalate "\n -" (l : map toString list))

warn :: (Log m) => String -> m ()
warn = log . withColor warningColor

alert :: (Log m) => String -> m ()
alert = log . withColor errorColor

errorColor :: String
errorColor = "\x1b[31m"

successColor :: String
successColor = "\x1b[32m"

warningColor :: String
warningColor = "\x1b[33m"

noneColor :: String
noneColor =
  "\x1b[0m"

darkGray :: String
darkGray = "\x1b[90m"

magenta :: String
magenta = "\x1b[95m"