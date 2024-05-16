{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Log where

import Relude

class Log m where
  log :: String -> m ()

withColor :: Color -> String -> String
withColor c x = toColor c <> x <> noneColor

infoListEntry :: (Log m, ToString a) => a -> m ()
infoListEntry name = log $ withColor Magenta (" - " <> toString name <> ":")

label :: (Log m) => String -> m ()
label name = info (name <> ":")

info :: (Log m) => String -> m ()
info = log . withColor Green

infoList :: (ToString a, Log m) => String -> [a] -> m ()
infoList l list = info (intercalate "\n -" (l : map toString list))

warn :: (Log m) => String -> m ()
warn = log . withColor Yellow

alert :: (Log m) => String -> m ()
alert = log . withColor Red

successColor :: String
successColor = "\x1b[32m"

warningColor :: String
warningColor = "\x1b[33m"

noneColor :: String
noneColor =
  "\x1b[0m"

data Color
  = Red
  | Green
  | Yellow
  | Gray
  | Magenta

toColor :: Color -> String
toColor c = "\x1b[" <> show (colorCode c) <> "m"

colorCode :: Color -> Int
colorCode Red = 31
colorCode Green = 32
colorCode Yellow = 33
colorCode Gray = 90
colorCode Magenta = 95