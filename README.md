# morpheus-graphql

Build GraphQL APIs with your favourite functional language!

morpheus graphql helps you to build GraphQL API in Haskell with native haskell types,
all your resolvers are regular haskell functions, morpheus graphql will convert your haskell schema to graphql schema.

See example for more details on at https://github.com/nalchevanidze/morpheus-graphql/tree/master/example

## Exisiting Features

- Introspection
- Enum
- Scalar
- InputObject
- Mutation

# Getting Started

define schema with native Haskell Types and derive them as GraphQL Schema and Introspection

starting point in morpheus graphql is to define your api function with morpheus interpreter
acourding your Query and Mutation type interpreter will generate GQL schema that could be introspecteb by request
for simplicity we wil not define mutation just query

```haskell
gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter
    GQLRoot {
      query = Query {  -- query resolver function
        user = resolveUser
      },
      mutation = () -- no mutation
    }

data Query = Query
  { user :: () ::-> User -- Field With No Arguments and IO interaction
  } deriving (Show, Data, Genneric , GQLQuery)
```

as you see Query type in morpheus graphql is just Haskell record, we derive it with **GQLQuery** as as Graphql Query
in this case our query has only one field user with no arguments **"()"** and Type **"User"**

anotation **"::->"** is inline haskell data Type with Constructor

```haskell
Resolver (argument -> IO Either string value)
```

arguments are also just haskell record with GQLArgs derivation

```haskell
-- Query Arguments
data Location = Location
  { zipCode :: Maybe Int -- Optional Argument
  , name  :: Text -- Required Argument
  } deriving (Show, Data, Generic , GQLArgs)
```

```haskell
data User = User
  { name    :: Text  -- not Null  Field
  , email   :: Maybe Text -- Nullable Field
  , address  :: Location ::-> Address -- Field With Arguments and IO interaction
  } deriving (Show, Data, Generic, GQLKind, GQLObject)

jsonUser :: IO (Either String JSONUser)
jsonUser = ...

-- Hi Order Resolver
resolveAddress :: JSONUser -> Location ::-> Address
resolveAddress = ...

resolveUser :: () ::-> User
resolveUser = Resolver $ const (jsonUser >>= \x -> return (buildResolverBy <$> x))
    where
        buildResolverBy user' =
            User {
                name = name user'
                , email = email user'
                , address = resolveAddress user'
            }
```

## Enum

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

## Scalar

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

## InputObject

inputObject can be used only inside in arguments or in another inputObject

```haskell

data Coordinates = Coordinates
{ latitude :: Int
, longitude :: Int
} deriving (Show, Generic, Data, GQLKind, GQLInput)

```

## Descriptions

if you need description for your GQL Type you can define GQL instance manualy and assign them description

```haskell
data Person = Person
{ name :: Text
} deriving (Show, Generic, Data, GQLInput)

instance GQLKind Person where
  description \_ = "ID of Cities in Zip Format"

```

# Mutation

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

## Roadmap

- Near future:
  - improve error handling
  - impove API
  - GQL isomorphic Introspection
- Medium future:
  - stabilize API
  - GQL isomorphic error handling
- Long term
  - Union Types
  - Query Alias
  - Performance Optimisation
