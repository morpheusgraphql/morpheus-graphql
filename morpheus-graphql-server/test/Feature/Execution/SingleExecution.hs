{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Feature.Execution.SingleExecution
  ( api,
  )
where

import Data.Morpheus.Server
import Data.Morpheus.Server.Types
import Data.Text (Text)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import GHC.Generics

data Query m = Query { test :: m Text } deriving (Generic, GQLType)

rootResolver :: MonadIO m => RootResolver m () Query Undefined Undefined
rootResolver = defaultRootResolver { queryResolver = Query { test = liftIO (putStrLn "ran") >> pure "hi" } }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
