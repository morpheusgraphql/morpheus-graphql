{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.NamedResolvers.Authors
  ( authorsApp,
    Author,
    Post,
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

instance ResolveNamed m Role where
  type Dep Role = ID
  resolveBatched = traverse getRole
    where
      getRole "1325" = pure (Just Admin)
      getRole _ = pure (Just Guest)

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

instance ResolveNamed m (Author (NamedResolverT m)) where
  type Dep (Author (NamedResolverT m)) = ID
  resolveBatched = traverse getAuthor
    where
      getAuthor uid =
        pure
          $ Just
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

instance ResolveNamed m (Post (NamedResolverT m)) where
  type Dep (Post (NamedResolverT m)) = ID
  resolveBatched = traverse getPost
    where
      getPost uid =
        pure
          $ Just
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

instance ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveBatched _ =
    pure
      [ Just
          Query
            { authors = resolve (pure ["1325", "2525"]),
              authorById = \(Arg uid) -> resolve (pure uid)
            }
      ]

authorsApp :: App () IO
authorsApp =
  deriveApp
    ( NamedResolvers ::
        NamedResolvers IO () Query Undefined Undefined
    )
