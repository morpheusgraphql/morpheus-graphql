{-# LANGUAGE OverloadedStrings #-}

module Server.MonadIO.Scotty where

import Control.Concurrent.STM
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, asks, runReader, runReaderT)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Morpheus (interpreter)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Tuple.Extra (both)
import Network.HTTP.Types (Status (..))
import Server.MonadIO.API
import Web.Scotty

-------------------------------------------------------------------------------
apiScotty :: B.ByteString -> Web B.ByteString
apiScotty = interpreter rootResolver

app :: IO ()
app = do
  db <- newTVarIO dbInit
  scotty 8080 $ post "/apiScotty" $ do
    reqBody <- body
    reqHeaders <- headers
    let env = Env db $ map (both $ T.pack . LT.unpack) reqHeaders
    res <- liftIO . runExceptT . flip runReaderT env . runWeb $ apiScotty reqBody
    case res of
      Left code -> status $ Status code "Error"
      Right rawResponse -> raw rawResponse
