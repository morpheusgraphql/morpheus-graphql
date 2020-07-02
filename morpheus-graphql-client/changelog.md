# Changelog

## 0.14.0 - Unreleased Changes

### new features

- support of `schema`. issue #412

  ```graphql
  schema {
    query: MyQuery
  }
  ```

## 0.13.0 - 22.06.2020

### breaking changes

- from now you should provide for every custom graphql scalar definition coresponoding haskell type definition and `GQLScalar` implementation fot it. for details see [`examples-client`](https://github.com/morpheusgraphql/morpheus-graphql/tree/master/examples-client)

- input fields and query arguments are imported without namespacing

### new features

- exposed: `ScalarValues`,`GQLScalar`, `ID`

## 0.12.0 - 21.05.2020
