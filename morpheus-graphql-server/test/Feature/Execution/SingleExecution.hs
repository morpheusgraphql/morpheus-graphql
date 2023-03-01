{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Morpheus.Server
import Data.Morpheus.Server.Types
import Data.Text (Text)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import GHC.Generics

data Query m = Query { test :: m Text } deriving (Generic, GQLType)

resolver :: MonadIO m => RootResolver m () Query Undefined Undefined
resolver = defaultRootResolver { queryResolver = Query { test = liftIO (putStrLn "ran") >> pure "hi" } }

main :: IO ()
main = B.getContents >>= interpreter resolver >>= B.putStrLn