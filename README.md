# morpheus-graphql

Build GraphQL APIs with your favourite functional language!

# Example

define schema with native Haskell Types and derive them automaticaly as GraphQL Types

```haskell
data CityID = Paris | BLN | HH deriving (Show, Data, Generic, GQLEnum)

data Coordinates = Coordinates {
    latitude :: Text,
    longitude :: Text
} deriving (Show, Data, Generic, GQLInput)

data LocationByCoordinates = LocationByCoordinates {
    coordinates :: Coordinates
} deriving (Show, Data, Generic, GQLArgs)

data Location = Location {
    zipCode:: Maybe Int, -- optional argument
    cityID:: EnumOf CityID -- Enum Value
} deriving (Show, Data, Generic, GQLArgs)

data Address = Address {
  city :: Text,
  house:: Int,
} deriving (Show, Data, Generic, GQLSelection)

data User = User {
  name :: Text,
  address:: LocationByCoordinates ::-> Address,
  office:: Location ::-> Maybe Address,
} deriving (Show, Data, Generic, GQLSelection )

newtype Query = Query {
  user:: () ::-> User
} deriving (Show, Data, Generic, GQLQuery )

newtype Mutation = Mutation {
  createUser:: LocationByCoordinates ::-> User
} deriving (Show, Data, Generic, GQLMutation)

```

# Resolvers

resolvers are haskell functions, they automaticaly recieve typed data as input

```haskell
resolveAddress :: LocationByCoordinates ::-> Address
resolveAddress = Resolver resolve $ getAdress -- ... your resolver function

resolveOffice :: User -> Location ::-> Address
resolveOffice user = Resolver resolve $ getAdress -- ... your resolver

resolveUser :: () ::-> User
resolveUser = Resolver resolve
 where
  resolve _ = pure $ User {
    name = "<name>",
    address = resolveAddress,
    office = resolveOffice user
  }

createUserMutation :: LocationByCoordinates ::-> User
createUserMutation = Resolver resolve -- your mutation function

resolve :: B.ByteString -> IO GQLResponse
resolve = interpreter GQLRoot
  { queryResolver    = Query { user = resolveUser }
  , mutationResolver = Mutation { createUser = createUserMutation }
  }
```
