{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.ServantGQL
  ( GQLEndpoint,
    serveGQLEndpoint,
  )
where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Morpheus.Types (GQLRequest, GQLResponse)
import Data.String (IsString)
import Data.Typeable (Typeable)
import GHC.TypeLits
import Network.HTTP.Media ((//), (/:))
import Servant
  ( (:<|>) (..),
    (:>),
    Accept (..),
    Get,
    JSON,
    MimeRender (..),
    Post,
    ReqBody,
    Server,
  )

data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]

instance MimeRender HTML ByteString where
  mimeRender _ = id

type GQLAPI = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

type GQLEndpoint (name :: Symbol) = name :> (GQLAPI :<|> Get '[HTML] ByteString)

serveGQLEndpoint :: (GQLRequest -> IO GQLResponse) -> Server (GQLEndpoint name)
serveGQLEndpoint app = (liftIO . app) :<|> playground

link :: (IsString a, Semigroup a) => a -> a -> a
link rel href = "<link rel=\"" <> rel <> "\"  href=\"" <> href <> "\" />"

meta :: (IsString a, Monoid a) => [(a, a)] -> a
meta attr = "<meta " <> mconcat (map renderAttr attr) <> " />"
  where
    renderAttr (name, value) = name <> "=\"" <> value <> "\" "

tag :: (Monoid a, IsString a) => a -> [a] -> a
tag name children = "<" <> name <> ">" <> mconcat children <> "</" <> name <> ">"

playground :: (Monad m, MonadIO m) => m ByteString
playground =
  pure $
    "<!DOCTYPE html>"
      <> tag
        "html"
        [ tag
            "head"
            [ meta [("charset", "utf-8")],
              meta
                [ ("name", "viewport"),
                  ("content", "user-scalable=no, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, minimal-ui")
                ],
              tag "title" ["GraphQL Playground"],
              link "stylesheet" "//cdn.jsdelivr.net/npm/graphql-playground-react/build/static/css/index.css",
              link "shortcut icon" "//cdn.jsdelivr.net/npm/graphql-playground-react/build/favicon.png"
            ],
          tag
            "body"
            [ "<script  \
              \         src=\"//cdn.jsdelivr.net/npm/graphql-playground-react/build/static/js/middleware.js\"> \
              \      </script> \
              \    </head>"
                <> "      <div id='root'></div> \
                   \      <script> \
                   \        window.addEventListener('load', (_) => \
                   \          GraphQLPlayground.init(document.getElementById('root'), {}) \
                   \        ); \
                   \      </script>"
            ]
        ]
