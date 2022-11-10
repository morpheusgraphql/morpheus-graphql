{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.NamedResolvers.Entities where

import Data.Morpheus.Server.CodeGen.Internal
import Data.Morpheus.Server.Types
import Feature.NamedResolvers.Realms (Deity, Realm)

data Entity m
  = EntityDeity (m (Deity m))
  | EntityRealm (m (Realm m))
  deriving (Generic)

instance (Typeable m) => GQLType (Entity m) where
  type KIND (Entity m) = TYPE

data Query m = Query
  { entities :: m [Entity m],
    entity :: Arg "id" ID -> m (Maybe (Entity m))
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
