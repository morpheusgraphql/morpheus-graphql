# Changelog

see latest changes on [Github](https://github.com/morpheusgraphql/morpheus-graphql/releases)

## 0.19.0 - 21.03.2022

### Breaking Changes

- Pair fields changed from `key`, `value` to `_0`, `_1`

### Minor Changes

- support: `NonEmpty`, `Vector`, `Seq`
- fixes decoding of types without record syntax (including tuples) (#667, #659, #665)
- support Aeson 2.0

## 0.18.0 - 08.11.2021

### new Features

- `NamedResolvers` (experimental feature): typed Haskell approach of providing apollo
  like named resolvers, apps with NamedResolvers theoretically can be safely merged.

- `TypeGuards`: as an alternative for interfaces

- `TaggedArguments`: support of type level tagged argument definition. for example:

  Haskell field definition

  ```hs
  myField :: Arg "a" Int -> Arg "b" (Maybe Text) -> m Text
  ```

  will generate GraphQL field

  ```gql
  myField(a:Int!, b:String): String!
  ```

- deriving will merge function arguments. for example:

  for following data type definitions:

  ```hs
  data A = A { a1 :: Text, a2 :: Int} deriving (Show, Generic, GQLType)
  data B = B {b :: Text} deriving (Show, Generic, GQLType)
  ```

  Haskell field definition

  ```hs
  myField :: A -> B -> m Text
  ```

  will generate GraphQL field

  ```gql
  myField(a1:String!, a2:Int!, b:String!): String!
  ```

### Breaking Changes

- non object variants constructors will be also unpacked
- removed `implements`field from `GQLType`
- removed `interface` from `Morpheus.Types`
- deprecated kind `INTERFACE`

## 0.17.0 - 25.02.2021

### new features

- (issue [#543](https://github.com/morpheusgraphql/morpheus-graphql/issues/543) & [#558](https://github.com/morpheusgraphql/morpheus-graphql/issues/558)): `GQLTypeOptions` supports new option `typeNameModifier`.
  Before the schema failed if you wanted to use the same type for input and output, and the user had no control over the eventual GraphQL type name of the generated schema. Now with this option you can
  provide a function of type `Bool -> String -> String` that generates a custom GraphQL type name. The first argument is a `Bool` that is `True` if the type is an input, and `False` otherwise. The second
  argument is a `String` representing the initial, auto-generated GraphQL type name. The function returns the desired type name. thanks @nalchevanidze & @bradsherman

  e.g this schema will not fail. morpheus will generate types: `Deity` and `InputDeity`

  ```hs
  data Deity = Deity
  { name :: Text,
    age :: Int
  }
  deriving (Show, Generic)

  deityTypeNameModifier isInput original
    | isInput = "Input" ++ original
    | otherwise = original

  instance GQLType Deity where
    typeOptions _ opt = opt {typeNameModifier = deityTypeNameModifier}

  newtype DeityArgs = DeityArgs
    { input :: Deity
    }
    deriving (Show, Generic, GQLType)

  newtype Query (m :: * -> *) = Query
    { deity :: DeityArgs -> m Deity
    }
    deriving (Generic, GQLType)
  ```

- exposed `EncodeWrapper` and `DecodeWrapper` type-classes.

### Breaking Changes

- `Map k v` is now represented as just `[Pair k v]`
- `GQLScalar` was replaced with `EncodeScalar` and `DecodeScalar` type-classes.
- Exclusive input objects: Sum types used as input types are represented as input objects, where only one field must have a value. Namespaced constructors (i.e., where referenced type name concatenated with union type name is equal to constructor name) are unpacked. Furthermore, empty constructors are represented as fields with the unit type.

  for example:

  ```hs
      data Device
        | DevicePC PC
        | Laptops { macAdress :: ID }
        | Smartphone
  ```

  this type will generate the following SDL:

  ```graphql
  enum Unit {
    Unit
  }

  input Laptop {
    macAdress: ID
  }

  input Device {
    PC: PC
    Laptops: Laptops
    Smartphone: Unit
  }
  ```

- For each nullary constructor will be defined GQL object type with a single field `_: Unit` (since GraphQL does not allow empty objects).

  for example:

  ```haskell
  data Person = Client { name :: Text } | Accountant | Developer
  ```

  this type will generate the following SDL:

  ```graphql
  enum Unit {
    Unit
  }

  type Student {
    name: String!
  }

  type Accountant {
    _: Unit!
  }

  type Developer {
    _: Unit!
  }

  union Person = Client | Accountant | Developer
  ```

- changed signature of `GQLType.typeOptions` from `f a -> GQLTypeOptions` to `f a -> GQLTypeOptions -> GQLTypeOptions`.

  now you can write:

  ```hs
    typeOptions _ options = options { fieldLabelModifier = <my function> }
  ```

whre argument options is default gql options.

- deexposed constructor of `GQLTypeOptions`.
- Type name for parametrized types like `One (Two Three)` will be generated directly, concatenating them `OneTwoThree` instead of `One_Two_Three.`
- Haskell `Float` was renamed to custom scalar type `Float32.`
- Haskell `Double` now represents GraphQL `Float`.

### Minor Changes

- deprecated kinds `INPUT`, `ENUM` and `OUTPUT` in favor of more generalized kind `TYPE`.
  now you can derive INPUT, ENUM and OUTPUT automatically with `deriving (Generic, GQLType)`.
- more likely to rebuild when a file loaded by `importGQLDocument` or
  `importGQLDocumentWithNamespace` is changed

## 0.16.0 - 05.11.2020

## Breaking changes

- subscriptions are extracted in `morpheus-graphql-subscriptions`.
- `Event`, `httpPubApp` and `webSocketsApp` moved `Data.Morpheus.Subscriptions`

## New Features

- `Data.Morpheus.Subscriptions` provides:

  - runPubApp: generalized version of `httpPubApp`
  - runSubApp: generalized version of `webSocketsApp`

- New encode and decode instances for `Set`, `NonEmpty`, `Seq` and `Vector`
  `Set` and `NonEmpty` throw a graphql error when a duplicate is found (Set)
  or when an empty list is sent (NonEmpty).
  **Beware**: Right now, all these types are advertised as lists in the introspection query.
  This is something we are trying to change by submitting a proposal to the graphql spec.

### Minor Changes

- parser performance optimization

## 0.15.1 - 12.09.2020

## 0.15.0 - 12.09.2020

### new features

- custom operation root types: e.g

  ```hs
  RootResolver IO () MyQuery MyMutation Undefined
  ```

  creates app with:

  ```graphql
  schema {
    query: MyQuery
    mutation: MyMutation
  }
  ```

- type : `App event m` and `deriveApp`

  ```hs
  app :: App EVENT IO
  app = runApp (deriveApp root)

  api :: a -> IO b
  api = runApp (deriveApp root)
  ```

- `App` supports semigroup(`schema Stitching`): if whe have two graphql apps

  ```hs
  mergedApi :: a -> m b
  mergedApi = runApp (deriveApp root <> deriveApp root2)
  ```

- `GQLType` exposes `typeOptions` to modify labels: `typeOptions :: f a -> GQLTypeOptions`

  where

  ```haskell
   GQLTypeOptions {
      fieldLabelModifier :: String -> String,
      constructorTagModifier :: String -> String
    }
  ```

- you can use `GQLType.getDescriptions` to document field or enum Values

- with `importGQLDocumentWithNamespace` now you can use Enums with Colliding Values:

  ```graphql
  enum X {
    A
  }

  enum Y {
    A
  }
  ```

  they will be namespaced to. `XA` and `YA`

### Breaking Changes

- `importGQLDocumentWithNamespace` they will be namespaced enum Values
- Argument types must have `GQLType` instances
- in `Data.Morpheus.Server`:

  - removed `subscriptionApp`
  - changed `webSocketsApp` type to `App e m -> m (ServerApp, e -> m ())`
  - changed `httpPubApp` type to `[e -> m ()] -> App e m -> a -> m b`

- removed `Stream` from `Data.Morpheus.Types`

- removed class `Interpreter`, `interpreter` is now just regular function.

  ```hs
  interpreter = runApp . deriveApp
  ```

### Minor Changes

- internal refactoring

## 0.14.1 - 16.08.2020

## 0.14.0 - 15.08.2020

### new features

- query validation supports interfaces
- `debugInterpreter`: displays internal context on graphql errors
- compileTimeSchemaValidation :
  morpheus validates schema at runtime (after the schema derivation).
  to be ensure that only correct api is compiled.
  we can use template haskell method `compileTimeSchemaValidation`

```hs
import Morpheus.Graphql.Server(compileTimeSchemaValidation)

_validateSchema :: ()
_validateSchema = $(compileTimeSchemaValidation (Identity gqlRoot))
```

- directive Validation for Document (TypeSystem).
- supports of block string values. e.g:

  ```graphql
  query {
    createDeity(
      name: """
      power qwe
      bla \n sd
      blu \\ date
      """
    ) {
      name
    }
  }
  ```

- `Data.Morpheus.Document` exposes `RootResolverConstraint`
- `Data.Morpheus.Server` exposes `httpPlayground`
- `httpPubApp` supports `GQLRequest -> GQLResponse`
- `morpheus-graphql-core` support of `schema`. issue #412

  ```graphql
  schema {
    query: Query
  }
  ```

  note that this does not affect `morpheus-graphql-server` at all. since it has its own schema derivation. you still need to provide:

  ```haskell
  rootResolver :: RootResolver () IO Query Undefined Undefined
  rootResolver = RootResolver <resolvers ...>
  ```

- Subscription Resolver supports `Monad`.
- nested Subscription Resolvers.

### Breaking Changes

- `Context' renamed to`ResolverContext'
- internal refactoring: changed AST
- root subscription fields must be wrapped with `SubscriptionField`. e.g:

```haskell
data Subscription (m :: * -> *) = Subscription
{ newDeity :: SubscriptionField (m Deity),
  newHuman :: HumanArgs -> SubscriptionField (m Human)
}
deriving (Generic)
```

- signature of `subscribe` is changed. now you can use it as followed:

```haskell
resolveNewAddress :: SubscriptionField (ResolverS EVENT IO Address)
resolveNewAddress = subscribe ADDRESS $ do
    -- executed only once
    -- immediate response on failures
    requireAuthorized
    pure $ \(Event _ content) -> do
        -- executes on every event
        lift (getDBAddress content)
```

- removed from `Data.Morpheus.Types`
  - `SubField`
  - `ComposedSubField`

## 0.13.0 - 22.06.2020

### breaking changes

- renamed `GQLRootResolver` -> `RootResolver`

### new features

- `importGQLDocument` automatically defines `GQLType` instances for scalar definitions
- supports default values

## 0.12.0 - 21.05.2020

### Breaking Changes

Package was extracted as:

- `morpheus-graphql-core`: core components like: parser, validator, executor, utils.

- Data.Morpheus.Core
- Data.Morpheus.QuasiQuoter
- Data.Morpheus.Error
- Data.Morpheus.Internal.TH
- Data.Morpheus.Internal.Utils
- Data.Morpheus.Types.Internal.Resolving
- Data.Morpheus.Types.Internal.Operation
- Data.Morpheus.Types.Internal.AST
- Data.Morpheus.Types.IO

- `morpheus-graphql-client`: lightweight version of morpheus client without server implementation

- Data.Morpheus.Client

- `morpheus-graphql`: morpheus graphql server
- Data.Morpheus
- Data.Morpheus.Kind
- Data.Morpheus.Types
- Data.Morpheus.Server
- Data.Morpheus.Document

deprecated:

- `Res`, `IORes`, `ResolveQ` : use `ResolverQ`
- `MutRes`, `IOMutRes`, `ResolveM` : use `ResolverM`
- `SubRes`, `IOSubRes`, `ResolveS`: use `ResolverS`
- `failRes`: use `MonadFail`

## New Feature

- `Semigroup` support for Resolver
- `MonadFail` Support for Resolver
- flexible resolvers: `ResolverO`, `ResolverQ` , `ResolverM`, `ResolverS`
  they can handle object and scalar types:

```hs
-- if we have record and regular Int
data Object m = Object { field :: m Int }

-- we can write
-- handles kind : (* -> *) -> *
resolveObject :: ResolverO o EVENT IO Object
-- is alias to: Resolver o () IO (Object (Resolver o () IO))
-- or
-- handles kind : *
resolveInt :: ResolverO o EVENT IO Int
-- is alias to: Resolver o () IO Int
```

the resolvers : `ResolverQ` , `ResolverM`, `ResolverS` , are like
`ResolverO` but with `QUERY` , `MUTATION` and `SUBSCRIPTION` as argument.

- flexible composed Resolver Type alias: `ComposedResolver`. extends `ResolverO` with
  parameter `(f :: * -> *)`. so that you can compose Resolvers e.g:

  ```hs
  resolveList :: ComposedResolver o EVENT IO [] Object
  -- is alias to: Resolver o () IO [Object (Resolver o () IO))]

  resolveList :: ComposedResolver o EVENT IO Maybe Int
  -- is alias to: Resolver o () IO (Maybe Int)
  ```

- server supports interfaces (see Readme):

  1. define interface with Haskell Types (runtime validation):
  2. define interface with `importGQLDocument` and `DSL` (compile time validation):

- support default directives: `@skip` and `@include`

- SelectionTree interface

### minor

- fixed subscription sessions, starting new session does not affects old ones.
- added tests for subscriptions

## 0.11.0 - 01.05.2020

### Breaking Changes

- Client generated enum data constructors are now prefixed with with the type name to avoid name conflicts.
- for Variant selection inputUnion uses `inputname` instead of `__typename`

- in `Data.Morpheus.Server`

  - `gqlSocketApp` and `gqlSocketMonadIOApp` are replaced with `webSocketsApp`
  - removed `initGQLState`, `GQLState`

- for better control of subscriptions

  - replaced instance `interpreter gqlRoot state` with
    `interpreter gqlRoot`.
  - added: `Input`, `Stream`, `httpPubApp`

  from now on you can define API that can be
  used in websockets as well as in http servers

  ```hs
  api :: Input api -> Stream api EVENT IO
  api = interpreter gqlRoot

  server :: IO ()
  server = do
    (wsApp, publish) <- webSocketsApp api
    let httpApp = httpPubApp api publish
    ...
    runBoth wsApp httpApp
  ```

  where `publish :: e -> m ()`

  websockets and http app do not have to be on the same server.
  e.g. you can pass events between servers with webhooks.

- subscription can select only one top level field (based on the GraphQL specification).

### New features

- Instead of rejecting conflicting selections, they are merged (based on the GraphQL specification).
- Support for input lists separated by newlines. thanks @charlescrain
- conflicting variable , fragment ... validation
- issue #411: Aeson `FromJSON` `ToJSON` instances for `ID`

### minor

- changes to internal types
- fixed validation of apollo websockets requests

## 0.10.0 - 07.01.2020

### Breaking Changes

- all constructors of `Resolver`: `QueryResolver`,`MutResolver`,`SubResolver` are unexposed. use `lift` , `publish` or `subscribe`.
  e.g

  ```hs
  -- Query Resolver
  resolveUser :: ResolveQ EVENT IO User
  resolveUser = lift getDBUser

  -- Mutation Resolver
  resolveCreateUser :: ResolveM EVENT IO User
  resolveCreateUser = do
    publish [userUpdate] -- publishes event inside mutation
    lift setDBUser

  -- Subscription Resolver
  resolveNewUser :: ResolveS EVENT IO User
  resolveNewUser = subscribe [USER] $ do
    pure $ \(Event _ content) -> lift (getDBUserByContent content)
  ```

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

- `type SubField` will convert your subscription monad to query monad.
  `SubField (Resolver Subscription Event IO) User` will generate same as
  `Resolver Subscription Event IO (User ((Resolver QUERY Event IO)))`

  now if you want define subscription as follows

  ```hs
  data Subscription m = Subscription {
    newUser :: SubField m User
  }
  ```

- `unsafeInternalContext` to get resolver context, use only if it really necessary.
  the code depending on it may break even on minor version changes.

  ```hs
  resolveUser :: ResolveQ EVENT IO User
  resolveUser = do
    Context { currentSelection, schema, operation } <- unsafeInternalContext
    lift (getDBUser currentSelection)
  ```

### minor

- monad instance for resolvers. thanks @dandoh
- example using stm, authentication, monad transformers. thanks @dandoh
- added dependency `mtl`

## [0.9.1] - 02.01.2020

- removed dependency `mtl`

## [0.9.0] - 02.01.2020

### Added

- `WithOperation` constraint for Generic Resolvers (#347) thanks @dandoh

### Fixed

- liftEither support in MutResolver (#351)
- selection of `__typename` on object und union objects (#337)
- auto inference of external types in gql document (#343)

  th will generate field `m (Type m)` if type has an argument

  e.g for this types and DSL

  ```hs
  data Type1 = Type1 { ... }
  type Type2 m = SomeType m
  data Type3 m = Type2 { bla :: m Text } deriving ...
  ```

  ```gql
  type Query {
    field1: Type1!
    field2: Type2!
    field3: Type3!
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
  data Deity m = Deity {
      name :: m Text
  }
  -- is equal to
  data Deity m = Deity {
      name :: () -> m Text
  }
  ```

- template haskell generates `m type` instead of `() -> m type` for fields without argument (#334)

  ```hs
  data Deity m = Deity {
      name :: (Arrow () (m Text)),
      power :: (Arrow () (m (Maybe Text)))
  }
  -- changed to
  data Deity m = Deity {
      name :: m Text,
      power :: m (Maybe Text)
  }
  ```

## [0.8.0] - 15.12.2019

### Changed

- deprecated: `INPUT_OBJECT`, `OBJECT`, `UNION`,

  - use `INPUT` instead of `INPUT_OBJECT`
  - use `deriving(GQLType)` instead of `OBJECT` or `UNION`

- only namespaced Unions generate regular graphql Union, other attempts will be wrapped inside an object with constructor name :

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
    CharacterDeity Deity -- Only <tyConName><conName> should generate direct link
  -- RECORDS
  | Creature { creatureName :: Text, creatureAge :: Int }
  --- Types
  | SomeDeity Deity
  | CharacterInt Int
  | SomeMulti Int Text
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
  | SomeMulti
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

type SomeMulti {
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

- for union records (`Creature { creatureName :: Text, creatureAge :: Int }`) will be referenced in union type, plus type `Creature`will be added in schema.

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

  - there is only types left with form `TypeName Type1 2Type ..`(e.g `SomeDeity Deity` ,`CharacterInt Int`, `SomeMulti Int Text`),

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

- on filed resolver was displayed. Unexhausted case exception of graphql error
- support of signed numbers (e.g `-4`)
- support of round floats (e.g `1.000`)
- validation checks undefined fields on inputObject
- variables are supported inside input values

## [0.7.1] - 26.11.2019

- max bound includes: support-megaparsec-8.0

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

  is helpful when you want to resolve GraphQL object

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
- deprecated `Data.Morpheus.Document.toGraphQLDocument`

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
- can be parsed default value on `inputObject`
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
  - `UserFriendPerson`: from `{user{friend`
  - `UserParentPerson`: from `{user{parent`
  - `UserBestFriendPerson`: from `{user{bestFriend`
  - `UserBestFriendParentPerson`: from `{user{bestFriend{parent`

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

- `String` defined in GQLDocument will be converted to `Text` by template haskell

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

- type family `KIND` is moved into typeClasses `GQLType`, so you should replace

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

```

```
