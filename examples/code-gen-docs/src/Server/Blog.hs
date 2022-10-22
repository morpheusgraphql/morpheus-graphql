{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Blog where

import Data.Data (Typeable)
import Data.Morpheus ()
import Data.Morpheus.Kind (TYPE)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Scalars

data Post m = Post
  { title :: m Text,
    body :: m (Maybe Markdown)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Post m) where
  type KIND (Post m) = TYPE

data User m = User
  { name :: m Text,
    posts :: m [Post m]
  }
  deriving (Generic)

instance (Typeable m) => GQLType (User m) where
  type KIND (User m) = TYPE

data Query m = Query
  { getUsers :: m [User m],
    getPosts :: m [Post m]
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
