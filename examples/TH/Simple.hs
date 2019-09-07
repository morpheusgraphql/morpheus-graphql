{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module TH.Simple
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocument)
import           Data.Morpheus.Types        (GQLRootResolver (..), IORes)
import           Data.Text                  (Text)

importGQLDocument "examples/TH/simple.gql"

rootResolver :: GQLRootResolver IO () () (Query IORes) () ()
rootResolver =
  GQLRootResolver {queryResolver = return Query {deity}, mutationResolver = pure (), subscriptionResolver = pure ()}
  where
    deity _deityArgs = pure Deity {name, power}
      where
        name _ = pure "Morpheus"
        power _ = pure (Just "Shapeshifting")

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
