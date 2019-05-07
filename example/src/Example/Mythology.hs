{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Example.Mythology
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (GQL, GQLArgs, GQLKind (..), GQLQuery, OBJECT)
import           Data.Morpheus.Types        ((::->) (..), GQLRoot (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

newtype Query = Query
  { deity :: DeityArgs ::-> Deity
  } deriving (Generic, GQLQuery)

type instance GQL Deity = OBJECT

data Deity = Deity
  { fullName :: Text -- Non-Nullable Field
  , power    :: Maybe Text -- Nullable Field
  } deriving (Generic)

instance GQLKind Deity where
  description _ = "Custom Description for Client Defined User Type"

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic, GQLArgs)

resolveDeity :: DeityArgs ::-> Deity
resolveDeity = Resolver $ \args -> dbDeity (name args) (mythology args)

dbDeity :: Text -> Maybe Text -> IO (Either String Deity)
dbDeity _ _ = return $ Right $ Deity {fullName = "Morpheus", power = Just "Shapeshifting"}

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter GQLRoot {query = Query {deity = resolveDeity}, mutation = ()}
