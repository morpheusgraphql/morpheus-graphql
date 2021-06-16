{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Api
  ( runApiTest,
  )
where

import Data.Morpheus.App
  ( App,
    mkApp,
  )
import Relude
import Test.Morpheus.Utils
  ( FileUrl (..),
  )
import Test.Tasty
  ( TestTree,
  )
import Utils.Utils
  ( assertValidSchema,
    getResolvers,
    testRequest,
  )

runApiTest :: FileUrl -> [FileUrl] -> [TestTree]
runApiTest url = map (testRequest (readApi url))

readApi :: Monad m => FileUrl -> IO (App e m)
readApi url = do
  schema <- assertValidSchema url
  resolvers <- getResolvers url
  pure $ mkApp schema resolvers
