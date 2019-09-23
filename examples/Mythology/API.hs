{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Mythology.API
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Types        (GQLRootResolver (..), GQLType, IORes, resolver)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Mythology.Character.Deity  (Deity (..), dbDeity)

newtype Query = Query
  { deity :: DeityArgs -> IORes Deity
  } deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic)

resolveDeity :: DeityArgs -> IORes Deity
resolveDeity args = resolver $ dbDeity (name args) (mythology args)

rootResolver :: GQLRootResolver IO () () Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {deity = resolveDeity}
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
