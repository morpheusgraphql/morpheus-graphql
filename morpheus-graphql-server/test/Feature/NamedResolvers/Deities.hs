{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.NamedResolvers.Deities where

import Data.Morpheus.Server.CodeGen.Internal
import Data.Morpheus.Server.Types

data Power
  = Shapeshifting
  | Thunderbolt
  deriving (Generic, Show)

instance GQLType Power where
  type KIND Power = TYPE

data Deity m = Deity
  { name :: m Text,
    power :: m [Power]
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Deity m) where
  type KIND (Deity m) = TYPE

data Query m = Query
  { deities :: m [Deity m],
    deity :: Arg "id" ID -> m (Maybe (Deity m))
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
