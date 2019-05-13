{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Mythology.Character.Deity
  ( Deity(..)
  , dbDeity
  ) where

import           Data.Morpheus.Kind     (GQLType (..), KIND, OBJECT)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Mythology.Place.Places (Realm (..))

type instance KIND Deity = OBJECT

instance GQLType Deity where
  description _ = "Custom Description for Client Defined User Type"

data Deity = Deity
  { fullName :: Text -- Non-Nullable Field
  , power    :: Maybe Text -- Nullable Field
  , realm    :: Realm
  } deriving (Generic)

dbDeity :: Text -> Maybe Text -> IO (Either String Deity)
dbDeity _ _ = return $ Right $ Deity {fullName = "Morpheus", power = Just "Shapeshifting", realm = Dream}
