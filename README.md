# Morpheus GraphQL

Build GraphQL APIs with your favourite functional language!

Morpheus GraphQL helps you to build GraphQL APIs in Haskell with native haskell types.
Morpheus will convert your haskell types to a GraphQL schema and all your resolvers are just native Haskell functions.

*Morpheus is still in an early stage of development, so any feedback is more than welcome, and we appreciate any contribution!
Just open an issue here on GitHub to get in contact.*

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
extra-deps:
  - morpheus-graphql-0.0.1
```

As Morpheus is quite new, make sure stack can find morpheus-graphql by running `stack update`

### Building your first API
To define a GraphQL API with Morpheus we start by defining the API Schema as a native Haskell data type,
 which derives the `Generic`and `GQLQuery` typeclasses. Lazily resolvable fields on this `Query` type are defined via the infix type `::->`,
 representing resolving a set of arguments `()` to a concrete value.
 
```haskell
data Query = Query
  { deity :: DeityArgs ::-> Deity
  } deriving (Generic , GQLQuery)
  
data Deity = Deity
  { fullname  :: Text          -- Non-Nullable Field
  , power     :: Maybe Text    -- Nullable Field
  } deriving (Generic, GQLKind, GQLObject, Typeable)
  
data DeityArgs = DeityArgs
  { name      :: Text        -- Required Argument
  , mythology :: Maybe Text  -- Optional Argument
  } deriving (Generic , GQLArgs)
```

For each field in the `Query` type defined via `::->` (like `deity`) we will define a resolver implementation that provides the values during runtime by referring to
some data source, e.g. a database or another API. Fields that are defined without `::->` you can just provide a value.

```haskell
resolveDeity :: DeityArgs ::-> Deity
resolveDeity = Resolver (\args -> do
  deity <- askDB (name args) (mythology args)
  return deity
)

askDB :: Text -> Maybe Text -> IO (Either String Deity)
askDB = ...
```
Note that the infix type `a ::-> b` is just syntactic sugar for `Resolver (a -> IO (Either String b))`


To make this `Query` type available as an API, we define a `GQLRoot` and feed it to the Morpheus `interpreter`. A `GQLRoot` consists
of `query` and `mutation` definitions, while we omit the latter for this example:

```haskell
gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter
    GQLRoot {
      query = Query {
        deity = resolveDeity
      },
      mutation = ()
    }
```
As you can see, the API is defined as `ByteString -> IO ByteString` which we can either invoke directly or use inside an arbitrary web framework
such as `scotty` or `serverless-haskell`.
```haskell
-- Using with scotty
main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)
```

## Advanced topics
### Enums

PLEASE DESCRIBE ENUMS HERE.

```haskell
data City
  = Hamburg
  | Paris
  | Berlin
  deriving (Show, Generic, Data, GQLKind , GQLEnum) -- GQL Enum

data SomeGQLType = SomeGQLType
  { ...
    ...
  , city  :: EnumOf City
  } deriving ...

-- pack Enum
SomeGQLType
  { ...
  , city = EnumOf Hamburg
  }

-- unpack Enum
getCity :: SomeGQLType -> City
getCity x = unpackEnum $ city x

```

### Scalar values

```haskell

data Odd = Int deriving (Show, Data, Generic, GQLKind)

instance GQLScalar Odd where
  parseValue (Int x) = pure $ Odd (...  )
  parseValue (String x) = pure $ Odd (...  )
  serialize  (Odd value) = Int value

data SomeGQLType = SomeGQLType { ....
 count :: ScalarOf Odd
} deriving ...

```

### InputObject
inputObject can be used only inside in arguments or in another inputObject

```haskell

data Coordinates = Coordinates
{ latitude :: Int
, longitude :: Int
} deriving (Show, Generic, Data, GQLKind, GQLInput)

```

### Field descriptions
if you need description for your GQL Type you can define GQL instance manually and assign them description

```haskell
data Person = Person
{ name :: Text
} deriving (Show, Generic, Data, GQLInput)

instance GQLKind Person where
  description \_ = "ID of Cities in Zip Format"

```

### Mutations

```haskell
newtype Mutation = Mutation
  { createUser :: Form ::-> User
  } deriving (Show, Generic, Data, GQLMutation)

createUser :: Form ::-> User
createUser = ...


gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter
    GQLRoot {
      query = Query {...},
      mutation = Mutation {
         createUser = createUser
      }
    }
```

### Introspection

  
# About

## The name
_Morpheus_ is the greek god of sleep and dreams whose name comes from the greek word _μορφή_ meaning form or shape.
He is said to be able to mimick different forms and GraphQL is good at doing exactly that: Transforming data in the shape
of many different APIs.

## Team
Morpheus is written and maintained by [_nalchevanidze_](https://github.com/nalchevanidze) and [_PygmalionPolymorph_](https://github.com/PygmalionPolymorph).

## Roadmap

- Medium future:
  - Stabilize API
  - Specification-isomorphic introspection
  - Specification-isomorphic error handling
- Long term:
  - Support all possible GQL features: Aliases, Unions, etc.
  - Performance optimization
