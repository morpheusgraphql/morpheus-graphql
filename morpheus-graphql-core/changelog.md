# Changelog

## 0.13.0 - Unreleased Changes

- exposed: `Data.Morpheus.Types.GQLScalar`
- exposed: `Data.Morpheus.Types.ID`


## 0.12.0 - 21.05.2020

## New features

- parser supports implemnets interfaces seperated with empty spaces

  ```gql
  type T implements A , B C & D {
  ```

- introspection can render interfaces
