# Morpheus GraphQL Core

core Functionalities of Morpheus GraphQL, can be used to build GraphQL server, client ..

- parser
- validar
- api

## Build GraphQL api with Core

```hs
getSchema :: Monad m => ResponseStream e m Schema
getSchema =
  fromList
    [dsl|
  type Query {
    deity(name: String): Deity!
  }

  type Deity {
    name: String!
    power: [String!]!
  }
|]

resolver :: Monad m => RootResModel e m
resolver =
  RootResModel
    { query =
        pure $
          mkObject
            "Query"
            [("deity", resolveDeity)],
      mutation = pure mkNull,
      subscription = pure mkNull
    }

resolveDeity :: (WithOperation o, Monad m) => Resolver o e m (ResModel o e m)
resolveDeity =
  pure $
    mkObject
      "Deity"
      [ ("name", pure $ mkString "Morpheus"),
        ("power", pure $ mkList [mkString "Shapeshifting"])
      ]

api :: GQLRequest -> ResponseStream e Identity (Value VALID)
api request = do
  schema <- getSchema
  runApi schema resolver request
```