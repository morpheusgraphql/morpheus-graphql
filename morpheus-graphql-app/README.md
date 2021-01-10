# Morpheus GraphQL App

provides executable application for server

- parser
- validar
- api

## Build GraphQL api with App

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
