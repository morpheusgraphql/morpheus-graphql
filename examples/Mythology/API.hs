{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE TypeOperators  #-}

module Mythology.API
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (GQLArgs, GQLQuery)
import           Data.Morpheus.Types        ((::->), GQLRoot (..), Resolver (..))
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

wrapIn :: forall a c. Either String a -> Either String (a, [c])
wrapIn x = (, []) <$> x

resolveDeity :: DeityArgs ::-> Deity
resolveDeity = Resolver $ \args -> wrapIn <$> dbDeity (name args) (mythology args)

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter GQLRoot {query = Query {deity = resolveDeity}, mutation = (), subscription = ()}
