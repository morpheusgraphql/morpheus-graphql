{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Log where

import Relude

class Log m where
  log :: String -> m ()

withColor :: String -> String -> String
withColor color x = color <> x <> noneColor

info :: (Log m) => String -> m ()
info = log . withColor successColor

infoList :: (ToString a, Log m) => String -> [a] -> m ()
infoList label list = info (intercalate "\n -" (label : map toString list))

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
