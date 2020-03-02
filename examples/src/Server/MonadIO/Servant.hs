{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.MonadIO.Servant where

import Control.Concurrent.STM
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson
import Data.Morpheus (interpreter)
import Data.Morpheus.Types
import Data.Text (Text)
import Network.Wai.Handler.Warp
import Servant
import Server.MonadIO.API hiding (Value)

type GraphqlAPI = Header "Authorization" Text :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

apiServant :: GQLRequest -> Web GQLResponse
apiServant = interpreter rootResolver

graphqlAPI :: Proxy GraphqlAPI
graphqlAPI = Proxy

app :: IO ()
app = do
  db <- newTVarIO dbInit
  run 8080 $ serve graphqlAPI (graphql db)

graphql :: TVar Database -> Maybe Text -> GQLRequest -> Handler GQLResponse
graphql db maybeToken request = do
  let headers = case maybeToken of
        Just tk -> [("Authorization", tk)]
        _ -> []
  let env = Env db headers
  res <- liftIO . runExceptT . flip runReaderT env . runWeb . apiServant $ request
  case res of
    Left errorCode -> throwError ServerError
      { errHTTPCode = errorCode,
        errReasonPhrase = "Error",
        errBody = "Error",
        errHeaders = []
      }
    Right response -> return response
