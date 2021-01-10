# Morpheus GraphQL App

provides utilities for creating executable GraphQL applications for servers. You can use it to create a schema-first GraphQL server with dynamic typings.

## Build schema-first GraphQL App with dynamic typings

```hs
schema :: Schema VALID
schema =
  [dsl|
  type Query {
    deity(name: String): Deity!
  }

  type Deity {
    name: String!
    power: [String!]!
  }
|]

resolver :: Monad m => RootResolverValue e m
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

resolveDeity :: Monad m => m (ResolverValue  m)
resolveDeity =
  pure $
    mkObject
      "Deity"
      [ ("name", pure $ mkString "Morpheus"),
        ("power", pure $ mkList [mkString "Shapeshifting"])
      ]

api :: ByteString -> IO  ByteString
api = runApp (mkApp schema resolver)
```
