# morpheus-graphql

Build GraphQL APIs with your favourite functional language!

# Example

define schema with native Haskell Types and derive them as GraphQL Types

check example project for more details

```haskell

data CityID
  = Paris
  | BLN
  | HH
  deriving (Show, Generic, Data, GQLEnum) -- GQL Enum

instance GQLKind CityID where
  description _ = "ID of Cities in Zip Format"

data Modulo7 =
  Modulo7 Int
          Int
  deriving (Show, Data, Generic, GQLKind)

instance GQLScalar Modulo7 where
  parseValue (Int x) = pure $ Modulo7 (x `div` 7) (x `mod` 7)
  parseValue _       = pure $ Modulo7 0 0
  serialize (Modulo7 value _) = Int value

data Coordinates = Coordinates
  { latitude  :: ScalarOf Modulo7
  , longitude :: Int
  } deriving (Show, Generic, Data, GQLInput) -- GQL Input Object

instance GQLKind Coordinates where
  description _ = "just random latitude and longitude"

data LocationByCoordinates = LocationByCoordinates
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Show, Generic, Data, GQLArgs) -- GQL Arguments

data Location = Location
  { zipCode :: Maybe Int
  , cityID  :: EnumOf CityID
  } deriving (Show, Data, Generic, GQLArgs) -- GQL Arguments

data Address = Address
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  , owner       :: Maybe User
  } deriving (Generic, Show, GQLKind, GQLObject, Data) -- GQL Object

data User = User
  { name    :: Text
  , email   :: Text
  , address :: LocationByCoordinates ::-> Address
  , office  :: Location ::-> Address
  , friend  :: () ::-> Maybe User
  , home    :: Maybe Address
  } deriving (Show, Generic, Data, GQLObject) -- GQL Object

instance GQLKind User where
  description _ = "Custom Description for Client Defined User Type"

newtype Query = Query
  { user :: () ::-> User
  } deriving (Show, Generic, Data, GQLQuery)

newtype Mutation = Mutation
  { createUser :: LocationByCoordinates ::-> User
  } deriving (Show, Generic, Data, GQLMutation)

```

# Resolvers

resolvers are haskell functions, they automaticaly recieve typed data as input

```haskell

jsonAddress :: IO (Either String JSONAddress)
jsonAddress = ...

jsonUser :: IO (Either String JSONAddress)
jsonUser = ...

toText :: Modulo7 -> Text
toText = ...

fetchAddress :: Modulo7 -> Text -> ResolveIO Address
fetchAddress (Modulo7 x y) streetName = lift jsonAddress >>= eitherToResponse modify
  where
    modify address =
      Address
        { city = ..
        , houseNumber = ...
        , street = ...
        , owner = ...
        }

resolveAddress :: LocationByCoordinates ::-> Address
resolveAddress = Resolver res
  where
    res args = fetchAddress (toText $ latitude $ coordinates args) (longitude $ coordinates args)

addressByCityID :: CityID -> Int -> ResolveIO Address
addressByCityID Paris code = fetchAddress ...
addressByCityID BLN code   = fetchAddress ...
addressByCityID HH code    = fetchAddress ...

resolveOffice :: JSONUser -> Location ::-> Address
resolveOffice _ = Resolver resolve'
  where
    resolve' args = addressByCityID (unpackEnum $ cityID args) ....

resolveUser :: () ::-> User
resolveUser = Resolver resolve'
  where
    resolve' _ = lift jsonUser >>= eitherToResponse modify
    modify user' =
      User
        { name = ...
        , email = ...
        , address = resolveAddress
        , office = resolveOffice user'
        , home = Nothing
        , friend = Resolver $ \_ -> pure Nothing
        }

createUserMutation :: LocationByCoordinates ::-> User
createUserMutation = Resolver resolve'
  where
    resolve' _ = lift jsonUser >>= eitherToResponse modify
    modify user' =
      User
        { name = ...
        , email = ...
        , address = resolveAddress
        , office = resolveOffice user'
        , home = Nothing
        , friend = Resolver $ \_ -> pure Nothing
        }

resolve :: B.ByteString -> IO GQLResponse
resolve =
  interpreter
    GQLRoot {
      queryResolver = Query {
        user = resolveUser
      },
      mutationResolver = Mutation {
        createUser = createUserMutation
      }
    }

```
