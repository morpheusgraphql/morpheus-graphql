{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Playground (playground) where

import Data.String (IsString)

link :: (IsString a, Semigroup a) => a -> a -> a
link rel href = "<link rel=\"" <> rel <> "\"  href=\"" <> href <> "\" />"

meta :: (IsString a, Monoid a) => [(a, a)] -> a
meta attr = t "meta" attr []

tag :: (Monoid a, IsString a) => a -> [a] -> a
tag tagName = t tagName []

t :: (Monoid a, IsString a) => a -> [(a, a)] -> [a] -> a
t tagName attr children =
  "<" <> tagName <> " " <> mconcat (map renderAttr attr) <> " >" <> mconcat children <> "</" <> tagName <> ">"
  where
    renderAttr (name, value) = name <> "=\"" <> value <> "\" "

script :: (Monoid a, IsString a) => [(a, a)] -> [a] -> a
script = t "script"

html :: (Monoid a, IsString a) => [a] -> a
html = ("<!DOCTYPE html>" <>) . t "html" []

playground :: (Monad m, Monoid a, IsString a) => m a
playground =
  pure $
    html
      [ tag
          "head"
          [ meta [("charset", "utf-8")],
            meta
              [ ("name", "viewport"),
                ("content", "user-scalable=no, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, minimal-ui")
              ],
            tag "title" ["GraphQL Playground"],
            link "stylesheet" "//cdn.jsdelivr.net/npm/graphql-playground-react/build/static/css/index.css",
            link "shortcut icon" "//cdn.jsdelivr.net/npm/graphql-playground-react/build/favicon.png",
            script
              [("src", "//cdn.jsdelivr.net/npm/graphql-playground-react/build/static/js/middleware.js")]
              []
          ],
        tag
          "body"
          [ t "div" [("id", "root")] [],
            script
              []
              [ "  window.addEventListener('load', (_) => \
                \    GraphQLPlayground.init(document.getElementById('root'), {}) \
                \  );"
              ]
          ]
      ]
