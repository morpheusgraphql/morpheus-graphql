{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
  ( main
  ) where

import           Control.Monad.IO.Class         (liftIO)
import           Data.Morpheus                  (Interpreter (..))
import           Data.Morpheus.Client           (defineQuery, gql)
import           Data.Morpheus.Document         (toGraphQLDocument)
import           Data.Morpheus.Server           (GQLState, gqlSocketApp, initGQLState)
import           Deprecated.API                 (Channel, Content, gqlRoot)
import           Mythology.API                  (mythologyApi)
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import           Network.WebSockets             (defaultConnectionOptions)
import           Web.Scotty                     (body, file, get, post, raw, scottyApp)

defineQuery
  [gql|
    query GetHero {
      deity {
         power
         fullName
        }
      hero {
         lifetime
      }
    }
  |]

instance Show GetHero where
  show _ = "Test show"

getHero :: String -> IO GetHero
getHero _ = return GetHero {getHeroDeity = "dsa", getHeroHero = "ass"}

main :: IO ()
main = do
  getHero "" >>= print
  state <- initGQLState
  httpApp <- httpServer state
  Warp.runSettings settings $ WaiWs.websocketsOr defaultConnectionOptions (wsApp state) httpApp
  where
    settings = Warp.setPort 3000 Warp.defaultSettings
    wsApp = gqlSocketApp gqlRoot
    httpServer :: GQLState IO Channel Content -> IO Wai.Application
    httpServer state =
      scottyApp $ do
        post "/" $ raw =<< (liftIO . interpreter gqlRoot state =<< body)
        get "/" $ file "examples/index.html"
        get "/schema.gql" $ raw (toGraphQLDocument gqlRoot)
        post "/mythology" $ raw =<< (liftIO . mythologyApi =<< body)
        get "/mythology" $ file "examples/index.html"
