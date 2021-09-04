{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.NamedResolvers.API
  ( app,
  )
where

import Data.Morpheus (deriveApp)
import Data.Morpheus.NamedResolvers
  ( ResolveNamed (..),
    maybeRef,
    refs,
    value,
  )
import Data.Morpheus.Types
  ( App,
    Arg (..),
    GQLType (..),
    ID,
    RootResolver (..),
    Undefined,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

fetchTitle :: Monad m => ID -> m Text
fetchTitle _ = pure "title"

postIDs :: Monad m => m [ID]
postIDs = pure ["1", "2"]

newtype PostType = PostType
  { pid :: Text
  }
  deriving (Generic, GQLType)

data Post m = Post
  { uid :: m ID,
    title :: m Text,
    postType :: m PostType
  }
  deriving (Generic, GQLType)

data Query m = Query
  { posts :: m [Post m],
    post :: Arg "id" ID -> m (Maybe (Post m))
  }
  deriving (Generic, GQLType)

instance ResolveNamed m Post where
  type Dep Post = ID
  resolveNamed args =
    pure $
      Just
        Post
          { uid = value (pure args),
            title = value (fetchTitle args),
            postType = value (pure PostType {pid = ""})
          }

instance ResolveNamed m Query where
  type Dep Query = ()
  resolveNamed () =
    pure $
      Just
        Query
          { posts = refs postIDs,
            post = \(Arg arg) -> maybeRef (pure arg)
          }

app :: App () IO
app =
  deriveApp
    (NamedResolvers :: RootResolver IO () Query Undefined Undefined)
