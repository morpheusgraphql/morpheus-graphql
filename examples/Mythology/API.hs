{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Mythology.API
  ( mythologyApi
  ) where

import           Control.Monad.Except       (ExceptT (..))
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Types        (Resolver (..), GQLRootResolver (..), GQLType, QUERY, Undefined (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Mythology.Character.Deity  (Deity (..), dbDeity)

newtype Query m = Query
  { deity :: DeityArgs -> m Deity
  } deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic)

resolveDeity :: DeityArgs -> IORes e Deity
resolveDeity args = QueryResolver $ ExceptT $ dbDeity (name args) (mythology args)

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = Query {deity = resolveDeity}
    , mutationResolver =  Undefined
    , subscriptionResolver = Undefined
    }

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
