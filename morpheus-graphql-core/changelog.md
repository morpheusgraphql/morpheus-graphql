# Changelog

## 0.14.0 - Unreleased Changes

### new features

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

- removed : `EventCon` from `Data.Morpheus.Core`
- internal refactoring: changed AST.
  Schema AST Types now need parameter `stage = RAW | CONST | VALID`.
  - `Schema VALID`
  - `TypeDefinition VALID`
  - `FieldDefinition IN VALID`
  - ...

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
