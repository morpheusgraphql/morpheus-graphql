---
layout: page
title: Named Resolvers
navOrder: 2
permalink: /named-resolvers/
---

## Named Resolvers

this feature requres minimum Morpheus GraphQL version **0.18.0**

_Posts.hs_

```hs
newtype Post m = Post
  { title :: m Text
  }
  deriving (Generic, GQLType)

instance Monad m => ResolveNamed m (Post (NamedResolverT m)) where
  type Dep (Post (NamedResolverT m)) = ID
  resolveNamed uid =
    pure Post { title = resolve (getPostTitle uid)}

-- QUERY
data Query m = Query
  { posts :: m [Post m],
    post :: Arg "id" ID -> m (Maybe (Post m))
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
        { posts = resolve getPostIds,
          post = \(Arg arg) -> resolve (pure (Just arg))
        }

postsApp :: App () IO
postsApp =
  deriveApp
    (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
```

_Authors.hs_

```hs
data Author m = Author
  { name :: m Text,
    posts :: m [Post m]
  } deriving (Generic, GQLType)

instance Monad m => ResolveNamed m (Author (NamedResolverT m)) where
  type Dep (Author (NamedResolverT m)) = ID
  resolveNamed uid =
    pure
      Author
        { name = resolve (getAuthorName uid),
          posts = resolve (getAuthorPosts uid)
        }

-- is alternative to extend type
newtype Post m = Post
  { author :: m (Author m)
  } deriving (Generic, GQLType)

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
  deriving (Generic, GQLType)

instance Monad m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed () =
    pure
      Query
        { authors = resolve getAuthorIds,
          authorById = \(Arg uid) -> resolve (pure (Just uid))
        }

authorsApp :: App () IO
authorsApp =
  deriveApp
    (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
```

_App.hs_

```hs
app :: App () IO
app = authorsApp <> postsApp
```

since the both `Post` type definitions have same dependency `ID`,
the interpreter safelly merge this two apps where type
`Post` will be extended with new field `author`.
