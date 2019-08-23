{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module TH.Simple
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (gqlDocument)
import           Data.Morpheus.Types        (GQLRootResolver (..), IORes)
import           Data.Text                  (Text)

[gqlDocument|

  type Query {
    deity (uid: Text! ) : Deity!
  }

  type Deity {
    name : Text!
    power    : Text
  }

|]

rootResolver :: GQLRootResolver IO () () Query () ()
rootResolver =
  GQLRootResolver {queryResolver = return Query {deity}, mutationResolver = pure (), subscriptionResolver = pure ()}
  where
    deity DeityArgs {uid} = pure Deity {name, power}
      where
        name _ = pure "Morpheus"
        power _ = pure (Just "Shapeshifting")

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
