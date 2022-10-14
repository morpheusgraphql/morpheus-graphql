{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Domains.Posts.Posts where

import Data.Data (Typeable)
import Data.Morpheus ()
import Data.Morpheus.Kind (TYPE)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Globals.GQLScalars

data Post m = Post
  { id :: m ID,
    title :: m Text,
    authorID :: m Text
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Post m) where
  type KIND (Post m) = TYPE
  directives _ =
    fieldDirective "title" Deprecated {reason = Nothing}
      <> fieldDirective "authorID" Deprecated {reason = Nothing}

data Query m = Query
  { posts :: m [Post m],
    post :: Arg "id" ID -> m (Maybe (Post m))
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
