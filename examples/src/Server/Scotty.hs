{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Server.Scotty
  ( scottyServer
  )
where


import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Morpheus.Document         ( toGraphQLDocument )
import           Data.Morpheus.Server           ( webSocketsApp
                                                , httpPubApp
                                                )
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.WebSockets
                                               as WaiWs
import           Network.WebSockets             ( defaultConnectionOptions )
import           Web.Scotty                     ( body
                                                , file
                                                , get
                                                , post
                                                , raw
                                                , scottyApp
                                                )

-- examples
import           Client.Client                  ( fetUser
                                                , fetchHero
                                                )
import           Server.Mythology.API           ( mythologyApi )
import           Server.TH.Simple               ( thSimpleApi )
import           Server.Sophisticated.API       ( EVENT
                                                , api
                                                , gqlRoot
                                                )

scottyServer :: IO ()
scottyServer = do
  (wsApp, publish) <- webSocketsApp api
  httpApp <- httpServer publish
  fetchHero >>= print
  fetUser (httpPubApp api publish) >>= print
  Warp.runSettings settings
    $ WaiWs.websocketsOr defaultConnectionOptions wsApp httpApp
 where
  settings = Warp.setPort 3000 Warp.defaultSettings
  httpServer :: (EVENT ->  IO ())  -> IO Wai.Application
  httpServer publish = scottyApp $ do
    post "/" $ raw =<< (liftIO . httpPubApp api publish =<< body)
    get "/" $ file "./examples/index.html"
    get "/schema.gql" $ raw $ toGraphQLDocument $ Identity gqlRoot
    post "/mythology" $ raw =<< (liftIO . mythologyApi =<< body)
    get "/mythology" $ file "./examples/index.html"
    post "/th" $ raw =<< (liftIO . thSimpleApi =<< body)
    get "/th" $ file "./examples/index.html"