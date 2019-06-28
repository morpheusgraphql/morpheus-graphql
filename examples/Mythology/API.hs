{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Mythology.API
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Types        (BaseR, GQLRootResolver (..), Resolver (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Mythology.Character.Deity  (Deity (..), dbDeity)

newtype Query = Query
  { deity :: DeityArgs -> BaseR Deity
  } deriving (Generic)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic)

resolveDeity :: DeityArgs -> BaseR Deity
resolveDeity args = Resolver $ dbDeity (name args) (mythology args)

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi =
  interpreter
    GQLRootResolver {queryResolver = Query {deity = resolveDeity}, mutationResolver = (), subscriptionResolver = ()}
