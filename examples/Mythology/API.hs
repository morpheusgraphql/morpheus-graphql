{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Mythology.API
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Types        (GQLRootResolver (..), ResM, gqlResolver)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Mythology.Character.Deity  (Deity (..), dbDeity)

newtype Query = Query
  { deity :: DeityArgs -> ResM Deity
  } deriving (Generic)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic)

resolveDeity :: DeityArgs -> ResM Deity
resolveDeity args = gqlResolver $ dbDeity (name args) (mythology args)

rootResolver :: GQLRootResolver IO () Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {deity = resolveDeity}
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
