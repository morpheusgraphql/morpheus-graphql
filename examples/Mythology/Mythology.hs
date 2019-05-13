{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module Example.Mythology
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (GQLArgs, GQLQuery)
import           Data.Morpheus.Types        ((::->) (..), GQLRoot (..))
import           Data.Text                  (Text)
import           Example.Character.Deity    (Deity (..), dbDeity)
import           GHC.Generics               (Generic)

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
mythologyApi = interpreter GQLRoot {query = Query {deity = resolveDeity}, mutation = (), subscription = ()}
