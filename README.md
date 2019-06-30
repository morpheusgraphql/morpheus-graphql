# Morpheus GraphQL

Build GraphQL APIs with your favourite functional language!

Morpheus GraphQL helps you to build GraphQL APIs in Haskell with native haskell types.
Morpheus will convert your haskell types to a GraphQL schema and all your resolvers are just native Haskell functions.

Morpheus is still in an early stage of development, so any feedback is more than welcome, and we appreciate any contribution!
Just open an issue here on GitHub, or join [our Slack channel](https://morpheus-graphql-slack-invite.herokuapp.com/) to get in touch.

## Getting Started

### Setup

To get started with Morpheus, you first need to add it to your project's dependencies, as follows (assuming you're using hpack):

package.yml

```yaml
dependencies:
  - morpheus-graphql
```

Additionally, you should tell stack which version to pick:

stack.yml

```yaml
resolver: lts-12.0 # or greater
extra-deps: # version 0.1.0 is not yet released(but will be soon), that why you should use Github source
  - megaparsec-7.0.5
  - github: morpheusgraphql/morpheus-graphql
    commit: 2ec9420c0c4c3013f039ca255080af7682d8d05f
```

As Morpheus is quite new, make sure stack can find morpheus-graphql by running `stack update`

### Building your first API

To define a GraphQL API with Morpheus we start by defining the API Schema as a native Haskell data type,
which derives the `Generic` typeclass. Lazily resolvable fields on this `Query` type are defined via the infix type `a -> ResM b`, representing resolving a set of arguments `a` to a concrete value `b`.

```haskell
data Query = Query
  { deity :: DeityArgs -> ResM Deity
  } deriving (Generic)

data Deity = Deity
  { fullName :: Text         -- Non-Nullable Field
  , power    :: Maybe Text   -- Nullable Field
  } deriving (Generic, GQLType)

type instance KIND Deity = OBJECT

data DeityArgs = DeityArgs
  { name      :: Text        -- Required Argument
  , mythology :: Maybe Text  -- Optional Argument
  } deriving (Generic)
```

For each field in the `Query` type defined via `a -> ResM b` (like `deity`) we will define a resolver implementation that provides the values during runtime by referring to
some data source, e.g. a database or another API. Fields that are defined without `a -> ResM b` you can just provide a value.

```haskell
resolveDeity :: DeityArgs -> ResM Deity
resolveDeity args = gqlResolver $ askDB (name args) (mythology args)

askDB :: Text -> Maybe Text -> IO (Either String Deity)
askDB = ...
```

Note that the type `a -> ResM b` is just Synonym for `a -> ExceptT String IO b`

To make this `Query` type available as an API, we define a `GQLRootResolver` and feed it to the Morpheus `interpreter`. A `GQLRootResolver` consists of `query`, `mutation` and `subscription` definitions, while we omit the latter for this example:

```haskell
rootResolver :: GQLRootResolver IO Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {deity = resolveDeity}
    , mutationResolver = return ()
    , subscriptionResolver = return ()
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
    fullname
    power
  }
}
```

our query will be resolved!

```JSON
{
  "data": {
    "deity": {
      "fullname": "Morpheus",
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

![Mythology Api](./assets/mythology-api.png "mythology-api")

## Advanced topics

### Enums

You can use Union Types as Enums, but they're not allowed to have any parameters.

```haskell
data City
  = Athens
  | Sparta
  | Corinth
  | Delphi
  | Argos
  deriving (Generic, GQLType)

type instance KIND City = ENUM
```

### Union types

To use union type, all you have to do is derive the `GQLType` class. Using GraphQL [_fragments_](https://graphql.org/learn/queries/#fragments), the arguments of each data constructor can be accessed from the GraphQL client.

```haskell
data Either b a
  = Right a
  | Left b
  deriving (Generic, GQLType)

type instance KIND City = UNION
```

### Scalar types

To use custom scalar types, you need to provide implementations for `parseValue` and `serialize` respectively.

```haskell
data Odd = Int deriving (Generic, GQLType)

instance GQLScalar Odd where
  parseValue (Int x) = pure $ Odd (...  )
  parseValue (String x) = pure $ Odd (...  )
  serialize  (Odd value) = Int value

type instance KIND Odd = SCALAR
```

### Applicative and Monad instance

The `Resolver` type has `Applicative` and `Monad` instances that can be used to compose resolvers.

### Introspection

Morpheus converts your schema to a GraphQL introspection automatically. You can use tools like `Insomnia` to take a
look at the introspection and validate your schema.
If you need a description for your GQLType inside of the introspection you can define the GQLType instance manually
and provide an implementation for the `description` function:

```haskell
data Deity = Deity
{ ...
} deriving (Generic)

instance GQLType Deity where
  description = const "A supernatural being considered divine and sacred"
```

screenshots from `Insomnia`

![alt text](./assets/introspection/spelling.png "spelling")
![alt text](./assets/introspection/autocomplete.png "autocomplete")
![alt text](./assets/introspection/type.png "type")

### Mutations

In addition to queries, Morpheus also supports mutations. The behave just like regular queries and are defined similarly:
Just exchange deriving `GQLQuery` for `GQLMutation` and declare them separately at the `GQLRootResolver` definition

```haskell
newtype Mutation = Mutation
  { createDeity :: Form -> EffectM Deity
  } deriving (Generic)

createDeityMutation :: Form ::-> Deity
createDeityMutation = ...

rootResolver :: GQLRootResolver IO Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {...}
    , mutationResolver = return Mutation {
       createDeity = createDeityMutation
    }
    , subscriptionResolver = return ()
    }

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver
```

### Subscriptions

because Subscriptions are in early stage of development at the time we just use `Text` as communication,
mutation with same chanel id will trigger subscription

```haskell
newtype Mutation = Mutation
  { createDeity :: Form -> EffectM Deity
  } deriving (Generic)

newtype Subscription = Mutation
  { newDeity :: Form -> EffectM Deity
  } deriving (Generic)

createDeityResolver :: a -> EffectM Address
createDeityResolver _ = gqlEffectResolver ["UPDATE_ADDRESS"] (fetchAddress (Euro 1 0))

newDeityResolver :: a -> EffectM Address
newDeityResolver _ = gqlEffectResolver ["UPDATE_ADDRESS"] $ fetchAddress (Euro 1 0)

rootResolver :: GQLRootResolver IO Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {...}
    , mutationResolver = return Mutation {
       createDeity = createDeityResolver
    }
    , subscriptionResolver = return Subscription {
         newDeity = newDeityResolver
      }
    }

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver
```

# About

## The name

_Morpheus_ is the greek god of sleep and dreams whose name comes from the greek word _μορφή_ meaning form or shape.
He is said to be able to mimic different forms and GraphQL is good at doing exactly that: Transforming data in the shape
of many different APIs.

## Team

Morpheus is written and maintained by [_nalchevanidze_](https://github.com/nalchevanidze) and [_PygmalionPolymorph_](https://github.com/PygmalionPolymorph).

## Roadmap

- Medium future:
  - Stabilize API
  - Specification-isomorphic introspection
  - Specification-isomorphic error handling
- Long term:
  - Support all possible GQL features
  - Performance optimization
