# morpheus-graphql

Build GraphQL APIs with your favourite functional language!

morpheus graphql helps you to build GraphQL API in Haskell with native haskell types,
all your resolvers are regular haskell functions, morpheus graphql will convert your haskell schema to graphql Introspection.

# Getting Started

Starting point in morpheus GraphQL is the definition of your API function with the morpheus interpreter.
according to your query and mutation type a GQL scheme and introspection will be generated.
for simplicity, we won't define mutation, we'll just define query.

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
  } deriving (Genneric , GQLQuery)
```

as you can see query type is just Haskell record, we derive it with **GQLQuery** as as Graphql Query. it has only one field user with no argument **"()"** and output Type **"User"**

notation **"::->"** is inline haskell data Type with Constructor **Resolver**

```haskell
Resolver (argument -> IO (Either String value))
```
where
- **string** is for error messages
- **value** value of field

arguments are Haskell record with GQLArgs derivation, as default all fields are required. only field with type Maybe is optional

```haskell
-- Query Arguments
data Location = Location
  { zipCode :: Maybe Int -- Optional Argument
  , name  :: Text -- Required Argument
  } deriving (Generic , GQLArgs)
```

for the GQL object, define the data record and derive it as a **GQLKind,GQLObject**.
only fields with **::->** are lazy and can access to IO. all other field will be evaluated instantly. by default all fields are notNull only (Maybe a) values are nullable.

```haskell
data User = User
  { name    :: Text  -- not Null  Field
  , email   :: Maybe Text -- Nullable Field
  , address  :: Location ::-> Address -- Field With Arguments and IO interaction
  } deriving (Typeable, Generic, GQLKind, GQLObject)
```

now we can write resolvers for your schema

```haskell
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

for more details you can See your Example on https://github.com/nalchevanidze/morpheus-graphql/tree/master/example

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

if you need description for your GQL Type you can define GQL instance manually and assign them description

```haskell
data Person = Person
{ name :: Text
} deriving (Show, Generic, Data, GQLInput)

instance GQLKind Person where
  description \_ = "ID of Cities in Zip Format"

```

## Mutation

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

# Existing Features

- Introspection
- Enum
- Scalar
- InputObject
- Mutation

# Roadmap

- Medium future:
  - stabile API
  - isomorphic Introspection
  - isomorphic error handling
- Long term
  - aLL possible GQL Types: Alias , Unions ..
  - performance optimisation
