## [0.3.1] - \*.2019

## [0.3.0] - 04.10.2019

### Added

- user can import GraphQL Document and generate types with it.

  ```haskell
    importGQLDocument "API.gql"
  ```

  this will generate types defined in `API.gql`

### Fixed

- `String` defined in GQLDcoument will be converted to `Text` by template haskell

- `importGQLDocument` and `gqlDocument` supports Mutation, Subscription and Resolvers with custom Monad

  for example. if we have:

  ```gql
  type Deity {
    name: String!
    power: Power!
  }
  ```

  where `Power` is another object defined by gql schema.
  template haskell will represent this type as:

  ```haskell
     data Deity m = Deity {
       name :: () -> m Text,
       power :: () -> m (Power m)
     }
  ```

  where `m` is resolver Monad.

- `importGQLDocumentWithNamespace` generates namespaced haskell records. so that you have no more problem with name collision.
  from this gql type:

  ```gql
  type Deity {
    name: (id:Int)String!
    power: Power!
  }
  ```

  will be generated.

  ```haskell
  data Deity m = Deity {
    deityName :: DeityNameArgs -> m Text,
    deityPower :: () -> m (Power m)
  }

  data DeityNameArgs = DeityNameArgs {
    deityNameArgsId :: Int
  }
  ```

### Changed

- `GQLType` is mandatory for every GQL Type (including Query, Mutation and Subscription)
- subscription Resolver changed

  from:

  ```haskell
    Subscription {newDeity = \args -> Event {channels = [ChannelA], content = newDeityResolver } }
  ```

  to:

  ```haskell
    Subscription {newDeity = \args -> SubResolver {subChannels = [ChannelA], subResolver = newDeityResolver } }
  ```

## [0.2.2] - 30.08.2019

### Fixed

- Parser Supports GraphQL multiline comments
- Morpheus GraphQL Client: Support GraphQL Alias
- Support of GraphQL Interfaces on GraphQL Document:

  ```gql
  # simple.gql
  interface Node {
    nodeId: ID!
  }

  type SimpleType implements Node {
    nodeId: ID!
    name: String!
  }
  ```

  morpheus compiler will read interfaces and validate implements.
  template haskell will generate haskell types only for types not for interfaces.

  haskell type from `simple.gql`:

  ```haskell
   data SimpleType = SimpleType {
      nodeId :: ID!
      name   :: Text!
    }  deriving (Generic)
  ```

  at the time compiler does not validates field Arguments by interface

## [0.2.1] - 23.08.2019

- assets are added to cabal source files

## [0.2.0] - 23.08.2019

### Added

- Parser Supports GraphQL comments
- Enhanced Subscription: mutation can trigger subscription with arguments
- Experimental Support of Input Unions
- GraphQL schema generating with: `Data.Morpheus.Document.toGraphQLDocument`
- Generating dummy Morpheus Api from `schema.gql`:

  ```
  morpheus build schema/mythology.gql src/MythologyApi.hs
  ```

  [details](https://github.com/morpheusgraphql/morpheus-graphql/issues/184)

- `convertToJSONName` & `convertToHaskellName` has been extended to support all Haskell 2010 reserved identities. [details](https://github.com/morpheusgraphql/morpheus-graphql/issues/207)

- `GraphQL Client` with Template haskell QuasiQuotes (Experimental, Not fully Implemented)

  ```haskell
  defineQuery
    [gql|
      query GetHero ($byRealm: Realm)
        {
          deity (realm:$byRealm) {
            power
            fullName
          }
        }
    |]
  ```

  will Generate:

  - response type `GetHero`, `Deity` with `Lens` Instances
  - input types: `GetHeroArgs` , `Realm`
  - instance for `Fetch` typeClass

  so that

  ```haskell
    fetchHero :: Args GetHero -> m (Either String GetHero)
    fetchHero = fetch jsonRes args
        where
          args = GetHeroArgs {byRealm = Just Realm {owner = "Zeus", surface = Just 10}}
          jsonRes :: ByteString -> m ByteString
          jsonRes = <fetch query from server>
  ```

  resolves well typed response `GetHero`.

- Ability to define `GQLSchema` with GraphQL syntax ,
  so that with this schema

  ```haskell

  [gqlDocument|
    type Query {
      deity (uid: Text! ) : Deity!
    }

    type Deity {
      name  : Text!
      power : Text
    }
  |]

  rootResolver :: GQLRootResolver IO () () Query () ()
  rootResolver =
    GQLRootResolver {queryResolver = return Query {deity}, mutationResolver = pure (), subscriptionResolver = pure ()}
    where
      deity DeityArgs {uid} = pure Deity {name, power}
        where
          name _ = pure "Morpheus"
          power _ = pure (Just "Shapeshifting")
  ```

  Template Haskell Generates types: `Query` , `Deity`, `DeityArgs`, that can be used by `rootResolver`

  generated types are not compatible with `Mutation`, `Subscription`,
  they can be used only in `Query`, but this issue will be fixed in next release

### Fixed:

- Parser supports enums inside input Object
- fulfilled fragment Validation (added: unusedFragment,nameConflict)
- correct decoding of Enums with more than 3 constructor #201

### Changed

- WebSocket subProtocol changed from `graphql-subscriptions` to `graphql-ws`

- type familiy `KIND` is moved into typeClasses `GQLType`, so you should replace

  ```haskell
  type instance KIND Deity = OBJECT

  instance GQLType Deity where
    description  = const "Custom Description for Client Defined User Type"

  data Deity = Deity { fullName :: Text } deriving (Generic)
  ```

  with

  ```haskell
  instance GQLType Deity where
  type KIND Deity = OBJECT
  description = const "Custom Description for Client Defined User Type"

  data Deity = Deity { fullName :: Text } deriving (Generic)
  ```

- Duplicated variable names in Http requests are validated using `Aeson`'s `jsonNoDup` function. So the following request will
  result in a parsing error

  ```
  {"query":"...",
  "variables":{"email":"foo@mail.net", "email":"bar@mail.net",...}}
  ```

## [0.1.1] - 1.07.2019

### Fixed:

- () as Subscription or Mutation does not defines Operator without fields

## [0.1.0] - 30.06.2019

thanks for contributing to: @krisajenkins, @hovind, @vmchale, @msvbg

### Added

- support for Union Types: `type instance KIND <type> = UNION`
- support of haskell Types: `Map`, `Set`, and Pair `(a,b)`
- GraphQL Resolver supports custom Monad
- add `Interpreter` class with instances:

  - `ByteString -> m ByteString` and Lazy `ByteString`, where `m` is resolver monad
  - `Text -> m Text` and Lazy `Text`, where `m` is resolver monad
  - `GQLRequest -> m GQLResponse` , When you using it inside another Component that have Manual `ToJSON` deriving,
    you have to ensure that `GQLResponse` will be encoded with `toEncoding`, and not with `toJSON`.

- Schema Validation:

  - Name Collision

- support of Parsing input values: `Objects`,`Arrays`
- support scalar type: `ID`
- scalar Types are validated by `GQLScalar` instance function `parseValue`
- TypeFamily `KIND` with:

  - `SCALAR`
  - `OBJECT`,
  - `ENUM`
  - `INPUT_OBJECT`
  - `UNION`

- inline Fragments
- GraphQL [Aliases](https://graphql.org/learn/queries/#aliases)
- Subscriptions: `GQLSubscription`

  - `a -> EffectM b` operation: is resolver that contains side effect in `EffectM`.
    is used for Mutation and Subscribe communication
  - `gqlEffectResolver ["CHANNEL_ID"]`: packs as effect Resolver.
    if mutation and subscription resolver have same channel then
    every call of mutation will trigger subscription resolver
  - `GQLState`: shared state between `http` and `websocket` server
  - `gqlSocketApp` :converts `interpreter` to `websocket` application
  - `graphql-subscriptions`: `Apollo GraphQL` subProtocol

- language:
  - Query supports : `__type(name:"type")`
  - On every Object can be selected : `__typename`

### Changed

- `GQLRootResolver`, `GQLType(..)` , `GQLScalar(..)`
  are moved in `Data.Morpheus.Types`
- `GQLRoot { query, mutation, subscription }` to `GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver}`
- `interpreter`: can be used in `http` and `websocket` server
- `GQLKind` renamed as `GQLType`
- types can be derived just with `(Generic,GQLType)`
- haskell record field `type'` will generate GQL Object field `type`
- public API (all other modules are hidden):
  - Data.Morpheus
  - Data.Morpheus.Kind
  - Data.Morpheus.Types
  - Data.Morpheus.Execution.Subscription

### Fixed:

- parser can read fields with digits like: a1 , \_1
- you can use Wrapped type and Wrapped Primitive Types issue #136:
  - wrapped TypesNames will be separated with "\_" : typeName(Either A B) -> "Either_A_B"
- introspection:
  - argument supports `Non-Null` and `List`
  - every field has correct kind

### Removed

- `GQLArgs`: you can derive arguments just with `Generic` without `GQLArgs`
- `GQLObject`: replaced with instance `type instance KIND <Type> = OBJECT`
- `GQLEnum`: replaced with instance `type instance KIND <Type> = ENUM`
- `GQLInput`: replaced with instance `type instance KIND <Type> = INPUT_OBJECT`
- `Typeable` : with new deriving it is not required anymore
- `Wrapper`: with TypeFamilies there is no need for `Wrapper`
- `a ::-> b` is Replaced by `a -> ResM b` where `ResM` is alias for `Resolver IO a`
- `GQLMutation` , `GQLQuery` : with new deriving it is not required anymore
- `Resolver` constructor replaced by functions:
  - `gqlResolver` : packs `m Either String a` to `Resolver m a`
  - `gqlEffectResolver`: resolver constructor for effectedResolver
  - `liftEffectResolver`: lifts normal resolver to Effect Resolver.
