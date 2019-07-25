{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Mythology.Character.Deity
  ( Deity(..)
  , dbDeity
  ) where

import           Data.Morpheus.Kind     (OBJECT)
import           Data.Morpheus.Types    (GQLType (..))
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Mythology.Place.Places (Realm (..))

data Deity = Deity
  { fullName :: Text -- Non-Nullable Field
  , power    :: Maybe Text -- Nullable Field
  , realm    :: Realm
  } deriving (Generic)

instance GQLType Deity where
  type KIND Deity = OBJECT
  description _ = "Custom Description for Client Defined User Type"

dbDeity :: Text -> Maybe Text -> IO (Either String Deity)
dbDeity _ _ = return $ Right $ Deity {fullName = "Morpheus", power = Just "Shapeshifting", realm = Dream}
