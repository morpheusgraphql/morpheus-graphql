module HConf.Utils.Chalk
  ( Color (..),
    chalk,
  )
where

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

chalk :: Color -> String -> String
chalk c x = toColor c <> x <> toColor None
