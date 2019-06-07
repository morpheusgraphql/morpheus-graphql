{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

module Mythology.API
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Types        ((::->), GQLArgs, GQLQuery, GQLRoot (..), Resolver (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Mythology.Character.Deity  (Deity (..), dbDeity)

newtype Query = Query
  { deity :: DeityArgs ::-> Deity
  } deriving (Generic, GQLQuery)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic, GQLArgs)

resolveDeity :: DeityArgs ::-> Deity
resolveDeity = Resolver $ \args -> dbDeity (name args) (mythology args)

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi =
  interpreter GQLRoot {queryResolver = Query {deity = resolveDeity}, mutationResolver = (), subscriptionResolver = ()}
