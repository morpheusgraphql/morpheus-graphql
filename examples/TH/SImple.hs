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
import           Data.Morpheus.Kind         (OBJECT)
import           Data.Morpheus.Types        (GQLRootResolver (..), IORes)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

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
    deity DeityArgs {uid} =
      pure $ Deity {name = const $ return "Morpheus", power = const $ return $ Just "Shapeshifting"}

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
