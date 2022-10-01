{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Playground
  ( httpPlayground,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Functor (fmap)
import Data.Semigroup ((<>))
import Prelude
  ( mconcat,
    (.),
  )

link :: ByteString -> ByteString -> ByteString
link rel href = "<link rel=\"" <> rel <> "\"  href=\"" <> href <> "\" />"

meta :: [(ByteString, ByteString)] -> ByteString
meta attr = t "meta" attr []

tag :: ByteString -> [ByteString] -> ByteString
tag tagName = t tagName []

t :: ByteString -> [(ByteString, ByteString)] -> [ByteString] -> ByteString
t tagName attr children =
  "<" <> tagName <> " " <> mconcat (fmap renderAttr attr) <> " >" <> mconcat children <> "</" <> tagName <> ">"
  where
    renderAttr (name, value) = name <> "=\"" <> value <> "\" "

script :: [(ByteString, ByteString)] -> [ByteString] -> ByteString
script = t "script"

html :: [ByteString] -> ByteString
html = docType . t "html" []

docType :: ByteString -> ByteString
docType x = "<!DOCTYPE html>" <> x

httpPlayground :: ByteString
httpPlayground =
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
