# Changelog

## 0.9.2 - *.*.2020

### New features

- exposed `publish` for mutation resolvers, now you can write

  ```hs
  resolveCreateUser :: ResolveM EVENT IO User
  resolveCreateUser = do
      requireAuthorized
      publish [userUpdate]
      liftEither setDBUser
  ```

- exposed `subscribe` for subscription resolvers, now you can write

  ```hs
  resolveNewUser :: ResolveS EVENT IO User
  resolveNewUser = subscribe [USER] $ do
      requireAuthorized
      pure userByEvent
    where userByEvent (Event _ content) = liftEither (getDBUser content)
  ```

### Minor

- MonadIO instance for resolvers. Thanks @dandoh
- Example using STM, authentication, monad transformers. Thanks @dandoh
- added dependency `mtl`

## [0.9.1] - 02.01.2020

- removed dependency `mtl`

## [0.9.0] - 02.01.2020

### Added

- `WithOperation` constraint for Generic Resolvers (#347) thanks @dandoh

### Fixed

- liftEither support in MutResolver (#351)
- selection of `__typename` on object und union objects (#337)
- auto inferece of external types in gql document (#343)

  th will generate field `m (Type m)` if type has an argument
  
  e.g for this types and DSL
  
  ```hs
  data Type1 = Type1 { ... }
  type Type2 m = SomeType m
  data Type3 m = Type2 { bla :: m Text } deriving ...
  ```

  ```gql
  type Query {
    field1 : Type1!
    field2 : Type2!
    field3 : Type3!
  }
  ```  

  morpheus generates

  ```hs
  data Query m = Query {
    field1 :: m Type1
    field2 :: m (Type2 m)
    field3 :: m (Type3 m)
  } deriving ...
  ```

  now you can combine multiple gql documents:
  
  ```hs
  importDocumentWithNamespace `coreTypes.gql`
  importDocumentWithNamespace `operations.gql`
  ```

### Changed

- support of resolver fields `m type` for the fields without arguments

  ```hs
  data Diety m = Deity {
      name :: m Text
  }
  -- is equal to
  data Diety m = Deity {
      name :: () -> m Text
  }
  ```

- template haskell generates `m type`  insead of `() -> m type` for fields without argument (#334)

  ```hs
  data Diety m = Deity {
      name :: (Arrow () (m Text)),
      power :: (Arrow () (m (Maybe Text)))
  }
  -- changed to
  data Diety m = Deity {
      name :: m Text,
      power :: m (Maybe Text)
  }
  ```

## [0.8.0] - 15.12.2019

### Changed

- deprecated: `INPUT_OBJECT`, `OBJECT`, `UNION`,

  - use `INPUT` instead of `INPUT_OBJECT`
  - use `deriving(GQLType)` insead of `OBJECT` or `UNION`

- only namespaced Unions  generate regular graphql Union, other attempts will be wrapped inside an object with constructor name :

  e.g:
  
  ```hs
  data Character =
    CharacterDeity Deity
    SomeDeity Deity
    deriving (GQLType)
  ```

  where `Deity` is Object.
  will generate

  ```gql
    union CHaracter = Deity | SomeDeity

    type SomeDeity {
      _0: Deity
    }
  ```

### Added

- `failRes` for resolver failures
- added kind: INPUT , OUTPUT
- Automatic Type Inference (only for Object, Union and Enum)
- More general stateful resolvers which accept instances of MonadIO (Authored by Sebastian Pulido [sebashack])
- Utility to create web-socket applications with custom MonadIO instances (Authored by Sebastian Pulido [sebashack])

```hs

data Realm  =
    Sky
  | Sea
  | Underworld
    deriving (Generic, GQLType)

data Deity  = Deity{
    fullName:: Text,
    realm:: Realm
  } deriving (Generic, GQLType)

data Character  =
    CharacterDeity Deity -- Only <tyconName><conName> should generate direct link
  -- RECORDS
  | Creature { creatureName :: Text, creatureAge :: Int }
  --- Types
  | SomeDeity Deity
  | CharacterInt Int
  | SomeMutli Int Text
  --- ENUMS
  | Zeus
  | Cronus deriving (Generic, GQLType)


```

will generate schema:

```gql
enum Realm {
  Sky
  Sea
  Underworld
}

type Deity {
  fullName: String!
  realm: Realm!
}

union Character =
    Deity
  | Creature
  | SomeDeity
  | CharacterInt
  | SomeMutli
  | CharacterEnumObject

type Creature {
  creatureName: String!
  creatureAge: Int!
}

type SomeDeity {
  _0: Deity!
}

type CharacterInt {
  _0: Int!
}

type SomeMutli {
  _0: Int!
  _1: String!
}

# enum
type CharacterEnumObject {
  enum: CharacterEnum!
}

enum CharacterEnum {
  Zeus
  Cronus
}
```

rules:

- haskell union type with only empty constructors (e.g `Realm`), will generate graphql `enum`
- haskell record without union (e.g `Deity`), will generate graphql `object`
- namespaced Unions: `CharacterDeity` where `Character` is TypeConstructor and `Deity` referenced object (not scalar) type: will be generate regular graphql Union

  ```gql
  union Character =
        Deity
      | ...
  ```

- for union recrods (`Creature { creatureName :: Text, creatureAge :: Int }`) will be referenced in union type, plus type `Creature`will be added in schema.

  e.g

  ```gql
    union Character =
      ...
      | Creature
      | ...

    type Creature {
      creatureName : String!
      creatureAge: Int!
    }

  ```

  - all empty constructors in union will be summed in type `<tyConName>Enum` (e.g `CharacterEnum`), this enum will be wrapped in `CharacterEnumObject` and this type will be added to union `Character`. as in example above

  - there is only types left with form `TypeName Type1 2Type ..`(e.g `SomeDeity Deity` ,`CharacterInt Int`, `SomeMutli Int Text`),

    morpheus will generate objet type from it:

    ```gql
    type TypeName {
      _0: Type1!
      _1: Type2!
      ...
    }
    ```

### Removed

- removed kind: INPUT_UNION

### Fixed

- on filed resolver was displayed. unexhausted case exception of graphql error
- support of signed numbers (e.g `-4`)
- support of round floats (e.g `1.000`) 
- validation checks undefined fields on inputObject
- variables are supported inside input values

## [0.7.1] - 26.11.2019

- max bound icludes: support-megaparsec-8.0

## [0.7.0] - 24.11.2019

### Removed

- `toMorpheusHaskellAPi` from `Data.Morpheus.Document` functionality will be migrated in `morpheus-graphql-cli`

### Changed

- `liftM` to `MonadTrans` instance method `lift`

- `liftEitherM` to `liftEither`

- `Resolver operation m event value` -> `Resolver operation event m value` , monad trans needs that last 2 type arguments are monad and value that why it was necessary

- exposed `Data.Morpheus.Types.Internal.AST`

- Mutation Resolver was changed from

```
resolver :: () -> ResolveM EVENT IO Address
resolver = MutResolver  {
  mutEvents = [someEventForSubscription],
  mutResolver = lift setDBAddress
}
```

```haskell
-- Mutation Wit Event Triggering : sends events to subscription
resolver :: () -> ResolveM EVENT IO Address
resolver = MutResolver \$ do
  value <- lift setDBAddress
  pure ([someEventForSubscription], value)
-- or
-- Mutation Without Event Triggering
resolver :: () -> ResolveM EVENT IO Address
resolver _args = lift setDBAddress
```

### Added

- added `parseDSL` to `Data.Morpheus.Document`

- GraphQL SDL support fully supports descriptions: onTypes, fields , args ...
  with (enums, inputObjects , union, object)
  for example :

  ```gql
  """
  Description for Type Address
  """
  type Address {
    """
    Description for Field city
    """
    city: String!
    street(
      """
      Description argument id
      """
      id: ID!
    ): Int!
  }
  ```

  ###### GraphQL SDL

  ```gql
  type User {
    name: String! @deprecated(reason: "some reason")
  }
  ```

  will displayed in introspection

  ###### introspection.json

  ```json
  {
    "data": {
      "__type": {
        "fields": [
          {
            "name": "city",
            "isDeprecated": true,
            "deprecationReason": "test deprecation field with reason"
          }
        ]
      }
    }
  }
  ```

- basic support of directive `@deprecated` on `enumValue` and object `field`, only on introspection

- GraphQL Client deprecation warnings

  on type

  ```gql
  type Human {
    humanName: String!
    lifetime: Lifetime! @deprecated(reason: "some reason")
    profession: Profession
  }
  ```

  compiler output:

  ```json
  warning:
    Morpheus Client Warning:
    {
      "message":"the field \"Human.lifetime\" is deprecated. some reason",
      "locations":[{"line":24,"column":15}]
    }
  ```

- new helper resolver types aliases:

  - ResolveQ : for Query
  - ResolveM : for Mutation
  - ResolveS : for Subscription

  `ResolveM EVENT IO Address` is same as `MutRes EVENT IO (Address (MutRes EVENT IO))`

  is helpfull wenn you want to resolve GraphQL object

### Fixed

- added missing Monad instance for Mutation resolver
- `defineByIntrospectionFile` does not breaks if schema contains interfaces
- Morpheus Client supports `Subscription` and `Mutation`operations

## [0.6.2] - 2.11.2019

### Added

- support of ghc 8.8.1

## [0.6.0] - 1.11.2019

### Removed

- removed `morpheus` cli for code generating, if you need cli you should use
  [morpheus-graphql-cli](https://github.com/morpheusgraphql/morpheus-graphql-cli/)

- example `API` executable is removed from Production build

### Added

- helper functions: `liftEitherM` , `liftM`

  ```haskell
    liftM :: m a -> Resolver o m e a
    liftEitherM :: m (Either String a) -> Resolver o m e a
  ```

## [0.5.0] - 31.10.2019

### Added

- dummy support of `directives`, only parsing not actual implementation

### Fixed

- can be parsed `implements` with multiple interfaces separated by `&`
- can be parsed default value on `inputobject`
- Parser supports anonymous Operation: `query` , `mutation` , `subscription`
  for example:

  ```gql
  mutation {
     name
  }
  ```

- Morpheus client does not breaks on `Boolean` type, converts every GraphQL type `Boolean` to haskell `Bool` and GQL `String` to `Text`

### Changed

- Reduced `GQLRootResolver` signature :

  `GQLRootResolver IO () () Query () ()` -> `GQLRootResolver IO () Query () ()`

  `GQLRootResolver IO Channel Content Query Mutation Subscription` -> `GQLRootResolver IO APIEvent Query Mutation Subscription`

  where `APIEvent = Event Channel Content`

- `GQLRootResolver` automatically assigns corresponding monad to GraphQL Types.

  you can write just:

  ```hs
  GQLRootResolver IO APIEvent Query  Mutation Subscription
  ```

  instead of:

  ```hs
  GQLRootResolver IO APIEvent (Query (Resolver IO))  (Mutation (MutResolver IO ApiEvent) (Subscription (SubResolver IO ApiEvent))
  ```

  where operations are generated by `importGQLDocument` or have form :

  ```
  data Query m = Query {
    field1 :: Args -> m Field1,
    ....
  }
  ```

- `()` was replaced with `Undefined` in `GQLRootResolver` for empty operations `mutation`, `subscription`
  ```
  rootResolver :: GQLRootResolver IO () Query Undefined Undefined
  ```
- Root Operations `Query`, `Mutation`, `Subscription` are passed to root resolvers without boxing inside a monad.
- there are only 3 kind of resolvers `MutResolver`, `SubResolver` , `QueryResolver` defined by GADT `Resolver`

## [0.4.0] - 09.10.2019

## Changed

- support of Default Value:

  - on query: Parsing Validating and resolving
  - on Document: only Parsing

- 'lens' is removed from Library, client field collision can be handled with GraphQL `alias`:
  ```gql
  {
    user {
      name
      friend {
        friendName: name
      }
    }
  }
  ```

### Fixed:

- `Data.Morpheus.Document.toGraphQLDocument` generates only my user defined types. #259
- Morpheus Client Namespaces Input Type Fields, they don't collide anymore:
  example:
  schema:

  ```gql
  input Person {
    name: String!
  }
  ```

  query:

  ```gql
  query GetUser (parent: Person!) {
    ....
  }
  ```

  wil generate:

  ```hs
  data GetUserArgs = GetUserArgs {
    getUserArgsParent: Person
  } deriving ...

  data Person = Person {
    personName: Person
  } deriving ...
  ```

- Morpheus Client Generated Output Object And Union Types don't collide:

  ```gql
  type Person {
    name: String!
    parent: Person!
    friend: Person!
  }
  ```

  And we select

  ```gql
  {
    user {
      name
      friend {
        name
      }
      parent {
        name
      }
      bestFriend: friend {
        name
        parent {
          name
        }
      }
    }
  }
  ```

  client will Generate:

  - `UserPerson` from `{user`
  - `UserFriendPerson`: from `{user{freind`
  - `UserParentPerson`: from `{user{parent`
  - `UserBestFriendPerson`: from `{user{bestFrend`
  - `UserBestFriendParentPerson`: from `{user{bestFrend{parent`

- GraphQL Client Defines enums and Input Types only once per query and they don't collide

## [0.3.1] - 05.10.2019

### Changed

- removed dependencies: attoparsec , utf8-string
- updated aeson lower bound up to: 1.4.4.0

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
