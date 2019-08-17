{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main
  ( main
  ) where

import           Control.Monad.IO.Class         (liftIO)
import           Data.Aeson                     (ToJSON (..))
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as L (readFile)
import           Data.Morpheus                  (Interpreter (..))
import           Data.Morpheus.Client           (Fetch (..), defineQuery, gql)
import           Data.Morpheus.Document         (toGraphQLDocument)
import           Data.Morpheus.Server           (GQLState, gqlSocketApp, initGQLState)
import           Deprecated.API                 (Channel, Content, gqlRoot)
import           GHC.Generics
import           Language.Haskell.TH.Quote
import           Mythology.API                  (mythologyApi)
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import           Network.WebSockets             (defaultConnectionOptions)
import           Web.Scotty                     (body, file, get, post, raw, scottyApp)

defineQuery
  [gql|
    query GetHero ($god: Realm, $charID: String!)
      {
        deity (mythology:$god) {
          power
          fullName
        }
        character(characterID: $charID ) {
          ...on Creature {
            creatureName
          }
          ...on Human {
            lifetime
            profession
          }
        }
      }
  |]

jsonRes :: ByteString -> IO ByteString
jsonRes req = do
  print req
  return
    "{\"deity\":{ \"fullName\": \"name\" }, \"character\":{ \"__typename\":\"Human\", \"lifetime\": \"Lifetime\", \"profession\": \"Artist\" }  }"

fetchHero :: Args GetHero -> IO (Either String GetHero)
fetchHero = fetch jsonRes

main :: IO ()
main = do
  fetchHero (GetHeroArgs {god = Just Realm {owner = "Zeus", surface = Just 10}, charID = "Hercules"}) >>= print
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
