# Changelog

## 0.15.0 - Unreleased Changes

### new features

- type : Api (..)

  ```hs
  api :: a -> m b
  api = runApi (mkApi schema resolvers)
  ```

- `Api` supports semigroup(`schema Stitching`):

  if whe have two apis `api1` and `api2` with type `Api EVENT IO` we can merge it as.

  ```hs
  mergedApi :: a -> m b
  mergedApi = runApi (api1 <> api2)
  ```

### Breaking Changes

- `runApi` changed signature to:

  ```hs
  runApi :: Api e m -> a -> m b
  ```

where you can api is product of resolver nd schema

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

- for better debuging, internal errors messages will display resolving state:
  - `current TypeName`
  - `current Selection`
  - `OperationDefinition`
  - `SchemaDefinition`
- rendering graphql "AST". e.g `render ( slection :: Selection VALID)` will render

```graphql
{
  user(arg1: 1) {
    name
  }
}
```

- quasiqouter `[dsl| <type definitions> |]` generates `Schema VALID`.
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
      powerqwe
      bla \n sd
      blu \\ dete
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

- parser supports implemnets interfaces seperated with empty spaces

  ```gql
  type T implements A , B C & D {
  ```

- introspection can render interfaces
