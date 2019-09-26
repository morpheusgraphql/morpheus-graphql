{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Main
  ( main
  ) where

import           Control.Monad.IO.Class         (liftIO)
import           Data.ByteString.Lazy           (ByteString)
import           Data.Functor.Identity          (Identity (..))
import           Data.Morpheus                  (Interpreter (..))
import           Data.Morpheus.Document         (toGraphQLDocument)
import           Data.Morpheus.Server           (GQLState, gqlSocketApp, initGQLState)
import           Mythology.API                  (mythologyApi)
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import           Network.WebSockets             (defaultConnectionOptions)
import           Sophisticated.API              (Channel, Content, gqlRoot)
import           TH.Simple                      (thSimpleApi)
import           Web.Scotty                     (body, file, get, post, raw, scottyApp)

fetUser :: GQLState IO Channel Content -> IO (Either String GetUser)
fetUser state = fetch (interpreter gqlRoot state) userArgs
  where
    userArgs :: Args GetUser
    userArgs = GetUserArgs {userCoordinates = Coordinates {longitude = [], latitude = String "1"}}

main :: IO ()
main = do
  state <- initGQLState
  httpApp <- httpServer state
  --fetchHero >>= print
  -- fetUser state >>= print
  Warp.runSettings settings $ WaiWs.websocketsOr defaultConnectionOptions (wsApp state) httpApp
  where
    settings = Warp.setPort 3000 Warp.defaultSettings
    wsApp = gqlSocketApp gqlRoot
    httpServer :: GQLState IO Channel Content -> IO Wai.Application
    httpServer state =
      scottyApp $ do
        post "/" $ raw =<< (liftIO . interpreter gqlRoot state =<< body)
        get "/" $ file "examples/index.html"
        get "/schema.gql" $ raw $ toGraphQLDocument $ Identity gqlRoot
        post "/mythology" $ raw =<< (liftIO . mythologyApi =<< body)
        get "/mythology" $ file "examples/index.html"
        post "/th" $ raw =<< (liftIO . thSimpleApi =<< body)
        get "/th" $ file "examples/index.html"
