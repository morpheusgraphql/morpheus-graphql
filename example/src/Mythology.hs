{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Mythology
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (GQLArgs, GQLKind (..), GQLObject, GQLQuery)
import           Data.Morpheus.Wrapper      ((::->) (..), GQLRoot (..))
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)

data Query = Query
  { deity :: DeityArgs ::-> Deity
  } deriving (Generic, GQLQuery)

data Deity = Deity
  { fullName :: Text -- Non-Nullable Field
  , power    :: Maybe Text -- Nullable Field
  } deriving (Generic, GQLObject, Typeable)

instance GQLKind Deity where
  description _ = "Custom Description for Client Defined User Type"

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic, GQLArgs)

resolveDeity :: DeityArgs ::-> Deity
resolveDeity = Resolver $ \args -> askDB (name args) (mythology args)

askDB :: Text -> Maybe Text -> IO (Either String Deity)
askDB _ _ = return $ Right $ Deity {fullName = "Morpheus", power = Just "Shapeshifting"}

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter GQLRoot {query = Query {deity = resolveDeity}, mutation = ()}
