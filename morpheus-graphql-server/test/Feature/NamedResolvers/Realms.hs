{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.NamedResolvers.Realms where

import Data.Morpheus.Server.CodeGen.Internal
import Data.Morpheus.Server.Types
import Feature.NamedResolvers.Scalars (Markdown)

data Realm m = Realm
  { name :: m Text,
    owner :: m (Deity m),
    description :: m Markdown
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Realm m) where
  type KIND (Realm m) = TYPE

newtype Deity m = Deity
  { realm :: m (Realm m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Deity m) where
  type KIND (Deity m) = TYPE

data Query m = Query
  { realms :: m [Realm m],
    realm :: Arg "id" ID -> m (Maybe (Realm m))
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
