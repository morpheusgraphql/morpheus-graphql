{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module API.Posts.Posts where

import Data.Data (Typeable)
import Data.Morpheus.Kind (TYPE)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Map (fromList, empty)

---- GQL Post -------------------------------
data Post m = Post
  { id :: m ID,
    title :: m Text,
    authorID :: m Text
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Post m) where
  type KIND (Post m) = TYPE

---- GQL Query -------------------------------
data Query m = Query
  { posts :: m [Post m],
    post :: Arg "id" ID -> m (Maybe (Post m))
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
