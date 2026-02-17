# Morpheus GraphQL [![Hackage](https://img.shields.io/hackage/v/morpheus-graphql.svg)](https://hackage.haskell.org/package/morpheus-graphql) ![CI](https://github.com/morpheusgraphql/morpheus-graphql/workflows/CI/badge.svg)

Build GraphQL APIs with your favorite functional language!

Morpheus GraphQL (Server & Client) helps you to build GraphQL APIs in Haskell with native Haskell types.
Morpheus will convert your Haskell types to a GraphQL schema and all your resolvers are just native Haskell functions. Morpheus GraphQL can also convert your GraphQL Schema or Query to Haskell types and validate them in compile time.

---

<sub><b>Note:</b> This project is developed and maintained using the <a href="https://github.com/nalchevanidze/hwm">hwm</a> monorepo tool, which automates builds, formatting, linting, and code generation for Haskell projects. If you're interested in efficient Haskell monorepo management, check out hwm!</sub>

Morpheus is still in an early stage of development, so any feedback is more than welcome, and we appreciate any contribution!
Just open an issue here on GitHub, or join [our Slack channel](https://morpheus-graphql.slack.com) to get in touch.

Please note that this readme file provides only a brief introduction to the library. If you are interested in more advanced topics, visit [Docs](https://morpheusgraphql.com/).

## Getting Started

### Setup

To get started with Morpheus, you first need to add it to your project's dependencies, as follows (assuming you're using hpack):

_package.yml_

```yaml
dependencies:
  - morpheus-graphql
  - morpheus-graphql-core
  - morpheus-graphql-subscriptions
```

Additionally, you should tell stack which version to pick:

_stack.yml_

```yaml
resolver: lts-16.2

extra-deps:
  - morpheus-graphql-0.28.0
  - morpheus-graphql-core-0.28.0
  - morpheus-graphql-app-0.28.0
  - morpheus-graphql-code-gen-0.28.0
  - morpheus-graphql-code-gen-utils-0.28.0
  - morpheus-graphql-server-0.28.0
  - morpheus-graphql-client-0.28.0
  - morpheus-graphql-subscriptions-0.28.0
```

### Building your first GraphQL API

### with GraphQL syntax

_schema.gql_

```gql
type Query {
  deity(name: String! = "Morpheus"): Deity!
}

"""
Description for Deity
"""
type Deity {
  """
  Description for name
  """
  name: String!
  power: String @deprecated(reason: "some reason for")
}
```

_API.hs_

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module API (api) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (RootResolver (..), Undefined (..))
import Data.Text (Text)

importGQLDocument "schema.gql"

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    deity DeityArgs {name} =
      pure
        Deity
          { name = pure name,
            power = pure (Just "Shapeshifting")
          }

api :: ByteString -> IO ByteString
api = interpreter rootResolver
```

Template Haskell Generates types: `Query` , `Deity`, `DeityArgs`, that can be used by `rootResolver`

`descriptions` and `deprecations` will be displayed in introspection.

`importGQLDocumentWithNamespace` will generate Types with namespaced fields. If you don't need namespace use `importGQLDocument`

### with Native Haskell Types

To define a GraphQL API with Morpheus we start by defining the API Schema as a native Haskell data type,
which derives the `Generic` type class. Using the `DeriveAnyClass` language extension we then also derive instances for the `GQLType` type class. Lazily resolvable fields on this `Query` type are defined via `a -> ResolverQ () IO b`, representing resolving a set of arguments `a` to a concrete value `b`.

```haskell
data Query m = Query
  { deity :: DeityArgs -> m Deity
  } deriving (Generic, GQLType)

data Deity = Deity
  { fullName :: Text         -- Non-Nullable Field
  , power    :: Maybe Text   -- Nullable Field
  } deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name      :: Text        -- Required Argument
  , mythology :: Maybe Text  -- Optional Argument
  } deriving (Generic, GQLType)
```

For each field in the `Query` type defined via `a -> m b` (like `deity`) we will define a resolver implementation that provides the values during runtime by referring to
some data source, e.g. a database or another API. Fields that are defined without `a -> m b` you can just provide a value.

In above example, the field of `DeityArgs` could also be named using reserved identities (such as: `type`, `where`, etc), in order to avoid conflict, a prime symbol (`'`) must be attached. For example, you can have:

```haskell
data DeityArgs = DeityArgs
  { name      :: Text        -- Required Argument
  , mythology :: Maybe Text  -- Optional Argument
  , type'     :: Text
  } deriving (Generic, GQLType)
```

The field name in the final request will be `type` instead of `type'`. The Morpheus request parser converts each of the reserved identities in Haskell 2010 to their corresponding names internally. This also applies to selections.

```haskell
resolveDeity :: DeityArgs -> ResolverQ () IO Deity
resolveDeity DeityArgs { name, mythology } = liftEither $ dbDeity name mythology

askDB :: Text -> Maybe Text -> IO (Either String Deity)
askDB = ...
```

To make this `Query` type available as an API, we define a `RootResolver` and feed it to the Morpheus `interpreter`. A `RootResolver` consists of `query`, `mutation` and `subscription` definitions, while we omit the latter for this example:

```haskell
rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity = resolveDeity}
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver
```

As you can see, the API is defined as `ByteString -> IO ByteString` which we can either invoke directly or use inside an arbitrary web framework
such as `scotty` or `serverless-haskell`. We'll go for `scotty` in this example:

```haskell
main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)
```

If we now send a POST request to `http://localhost:3000/api` with a GraphQL Query as body for example in a tool like `Insomnia`:

```GraphQL
query GetDeity {
  deity (name: "Morpheus") {
    fullName
    power
  }
}
```

our query will be resolved!

```JSON
{
  "data": {
    "deity": {
      "fullName": "Morpheus",
      "power": "Shapeshifting"
    }
  }
}
```

## Serverless Example

If you are interested in creating a `Morpheus GraphQL` API with `Serverless`, you should take a look at our example in this repository:
[_Mythology API_](https://github.com/morpheusgraphql/mythology-api) it is our example project build with `Morpheus GraphQL` and `Serverless-Haskell`,
where you can query different mythology characters with `GraphiQL`.

Mythology API is deployed on : [_api.morpheusgraphql.com_](https://api.morpheusgraphql.com) where you can test it with `GraphiQL`

![Mythology Api](https://morpheusgraphql.com/assets/img/mythology-api.png "mythology-api")

## Showcase

Below are the list of projects using Morpheus GraphQL. If you want to start using Morpheus GraphQL, they are
good templates to begin with.

- https://github.com/morpheusgraphql/mythology-api
  - Serverless Mythology API
- https://github.com/dandoh/web-haskell
  - Modern webserver boilerplate in Haskell: Morpheus Graphql + Postgresql + Authentication + DB migration + Dotenv and more

_Edit this section and send PR if you want to share your project_.

# About

## The name

_Morpheus_ is the greek god of sleep and dreams whose name comes from the greek word _μορφή_ meaning form or shape.
He is said to be able to mimic different forms and GraphQL is good at doing exactly that: Transforming data in the shape
of many different APIs.

## Team

Morpheus is written and maintained by [_nalchevanidze_](https://github.com/nalchevanidze)

## Roadmap

- Medium future:
  - Stabilize API
  - Specification-isomorphic error handling
- Long term:
  - Support all possible GQL features
  - Performance optimization
