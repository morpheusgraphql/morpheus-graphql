{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Rendering.TestSchemaRendering
  ( testSchemaRendering,
  )
where

import qualified Data.ByteString.Lazy.Char8 as T
import Data.Morpheus.Server (printSchema)
import Relude hiding (ByteString, readFile)
import Rendering.Schema (proxy)
import Test.Morpheus (file, mkUrl, renderingAssertion)
import Test.Tasty (TestTree)

xyz :: b -> IO (Either String T.ByteString)
xyz _ = pure $ Right $ printSchema proxy

testSchemaRendering :: TestTree
testSchemaRendering = renderingAssertion xyz (file (mkUrl "Rendering") "schema")
