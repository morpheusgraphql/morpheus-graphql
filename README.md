# morpheus-graphql

Build GraphQL APIs with your favourite functional language!

## Hello world GrapqhQL haskell

define schema with native Haskell Types and derive them as GraphQL Schema and Introspection

```haskell

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter
    GQLRoot {
      query = Query {
        user = resolveUser
      },
      mutation = () -- no mutation
    }

-- Query Arguments
data Location = Location
  { zipCode :: Maybe Int -- Optional Argument
  , name  :: Text -- Required Argument
  } deriving (Show, Data, Generic , GQLArgs)

data User = User
  { name    :: Text  -- not Null  Field
  , email   :: Maybe Text -- Nullable Field
  , address  :: Location ::-> Address -- Field With Arguments and IO interaction
  } deriving (Show, Data, Generic, GQLKind, GQLObject)

newtype Query = Query
  { user :: () ::-> User -- Field With No Arguments and IO interaction
  } deriving (Show, Data, Genneric , GQLQuery)


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
