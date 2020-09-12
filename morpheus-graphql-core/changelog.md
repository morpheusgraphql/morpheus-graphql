# Changelog

## 0.15.1 - 12.09.2020

relaxed upper boundary of `megaparsec` up to 10.0.0

## 0.15.0 - 12.09.2020

### new features

- `render` renders SchemaDefinition e.g

  ```graphql
  schema {
    query: MyQuery
  }
  ```

- query validator automatically adds `__typename` to interface types

- type : `App`

  ```hs
  api :: a -> m b
  api = runApp (mkApp schema resolvers)
  ```

- `App` supports semigroup(`schema Stitching`):

  if whe have two apps `app1` and `app2` with type `Api EVENT IO` we can merge it as.

  ```hs
  mergedApi :: a -> m b
  mergedApi = runApp (app1 <> app2)
  ```

- `runApp` changed signature to:

  ```hs
  runApp :: Api e m -> a -> m b
  ```

### Breaking Changes

- removed `runApi`.

### Minor Changes

- internal refactoring

## 0.14.1 - 16.08.2020

## 0.14.0 - 15.08.2020

### new features

- query validation supports interfaces
- exposed: `Data.Morpheus.Types.SelectionTree`
- configurable api: `Data.Morpheus.Core` exports

  - `Config`
  - `defaultConfig`
  - `debugConfig`

- for better debugging, internal errors messages will display resolving state:
  - `current TypeName`
  - `current Selection`
  - `OperationDefinition`
  - `SchemaDefinition`
- rendering graphql "AST". e.g `render (selection :: Selection VALID)` will render

```graphql
{
  user(arg1: 1) {
    name
  }
}
```

- quasiquoter `[dsl| <type definitions> |]` generates `Schema VALID`.
- parser supports custom directive definition. e.g

```graphql
directive @MyDirective on FIELD_DEFINITION | OBJECT
```

- directive Validation for Document (TypeSystem).
- supports of block string values. e.g:

  ```graphql
  query {
    createDeity(
      name: """
      power
      bla \n sd
      blu \\ date
      """
    ) {
      name
    }
  }
  ```

- support of `schema`. issue #412

  ```graphql
  schema {
    query: MyQuery
  }
  ```

### Breaking Changes

- `Context' renamed to`ResolverContext'
- removed : `EventCon` from `Data.Morpheus.Core`
- internal refactoring: changed AST.
  Schema AST Types now need parameter `stage = RAW | CONST | VALID`.
  - `Schema VALID`
  - `TypeDefinition VALID`
  - `FieldDefinition IN VALID`
  - ...
- runApi requires argument config

  ```hs
    runApi ::
      Schema s ->
      RootResModel event m ->
      Config ->
      GQLRequest ->
      ResponseStream event m (Value VALID)
  ```

## 0.13.0 - 22.06.2020

### new features

- exposed: `Data.Morpheus.Types.GQLScalar`
- exposed: `Data.Morpheus.Types.ID`
- finished interface validation
- supports default values

## minor changes

- internal refactoring
- added dependency `mtl`
- validates strings as enum from JSON value

## 0.12.0 - 21.05.2020

## New features

- parser supports implements interfaces separated with empty spaces

  ```gql
  type T implements A , B C & D {
  ```

- introspection can render interfaces
