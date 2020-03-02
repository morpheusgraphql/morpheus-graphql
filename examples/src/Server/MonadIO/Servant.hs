{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.MonadIO.Servant where

import Control.Concurrent.STM
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.Wai.Handler.Warp
import Servant
import Server.MonadIO.API hiding (Value)

type GraphqlAPI = Header "Authorization" Text :> ReqBody '[JSON] Value :> Post '[JSON] Value

graphqlAPI :: Proxy GraphqlAPI
graphqlAPI = Proxy

app :: IO ()
app = do
  db <- newTVarIO dbInit
  run 8080 $ serve graphqlAPI (graphql db)

graphql :: TVar Database -> Maybe Text -> Value -> Handler Value
graphql db maybeToken reqBody = do
  let headers = case maybeToken of
        Just token -> [("Authorization", token)]
        _ -> []
  let env = Env db headers
  res <- liftIO . runExceptT . flip runReaderT env . runWeb . api $ encode reqBody
  case res of
    Left errorCode -> throwError ServerError
      { errHTTPCode = errorCode,
        errReasonPhrase = "Error",
        errBody = "Error",
        errHeaders = []
      }
    Right rawResponse -> case decode rawResponse of
      Just response -> return response
      Nothing -> throwError err501
