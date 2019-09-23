{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module TH.Simple
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types        (GQLRootResolver (..), IORes)
import           Data.Text                  (Text)

importGQLDocumentWithNamespace "examples/TH/simple.gql"

rootResolver :: GQLRootResolver IO () () (Query IORes) () ()
rootResolver =
  GQLRootResolver
    {queryResolver = return Query {queryDeity}, mutationResolver = pure (), subscriptionResolver = pure ()}
  where
    queryDeity _deityArgs = pure Deity {deityName, deityPower}
      where
        deityName _ = pure "Morpheus"
        deityPower _ = pure (Just "Shapeshifting")

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
