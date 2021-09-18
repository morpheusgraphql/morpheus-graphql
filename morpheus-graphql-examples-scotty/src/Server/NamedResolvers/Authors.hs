{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.NamedResolvers.Authors
  ( authorsApp,
  )
where

import Data.Aeson.Types (ToJSON)
import Data.Morpheus (deriveApp)
import Data.Morpheus.NamedResolvers
  ( NamedResolverT,
    ResolveNamed (..),
    resolve,
  )
import Data.Morpheus.Types
  ( App,
    Arg (..),
    GQLType (..),
    ID,
    NamedResolvers (..),
    Undefined,
  )
import GHC.Generics (Generic)

data Role
  = Guest
  | Admin
  deriving
    ( GQLType,
      Generic,
      ToJSON
    )

instance Monad m => ResolveNamed m Role where
  type Dep Role = ID
  resolveNamed "1325" = pure Admin
  resolveNamed _ = pure Guest

-- AUTHOR
data Author m = Author
  { authorId :: m ID,
    role :: m Role,
    posts :: m [Post m]
  }
  deriving
    ( Generic,
      GQLType
    )

instance Monad m => ResolveNamed m (Author (NamedResolverT m)) where
  type Dep (Author (NamedResolverT m)) = ID
  resolveNamed uid =
    pure
      Author
        { authorId = resolve (pure uid),
          role = resolve (pure uid),
          posts = resolve (pure ["2321", "2112"])
        }

-- POST EXTENSION
newtype Post m = Post
  { author :: m (Author m)
  }
  deriving
    ( Generic,
      GQLType
    )

instance Monad m => ResolveNamed m (Post (NamedResolverT m)) where
  type Dep (Post (NamedResolverT m)) = ID
  resolveNamed uid =
    pure
      Post
        { author = resolve (pure uid)
        }

-- QUERY
data Query m = Query
  { authors :: m [Author m],
    authorById :: Arg "id" ID -> m (Maybe (Author m))
  }
  deriving
    ( Generic,
      GQLType
    )

instance Monad m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed () =
    pure
      Query
        { authors = resolve (pure ["1325", "2525"]),
          authorById = \(Arg uid) -> resolve (pure (Just uid))
        }

authorsApp :: App () IO
authorsApp =
  deriveApp
    (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
