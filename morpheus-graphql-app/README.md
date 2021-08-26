# Morpheus GraphQL App

provides utilities for creating executable GraphQL applications for servers. You can use it to create a schema-first GraphQL server with dynamic typings.

## Build schema-first GraphQL App with dynamic typings

```hs
schema :: Schema VALID
schema =
  [dsl|
  type Deity {
    name: String!
    power: [String!]
  }

  type Query {
    deity(id: ID): Deity
  }
|]

deityResolver :: Monad m => NamedResolverFunction QUERY e m
deityResolver (Scalar  "zeus") =
  [ ("name", pure "Zeus"),
    ("power", pure $ mkList [])
  ]
deityResolver _ =
  [ ("name", pure "Morpheus"),
    ("power", pure $ mkList [ "Shapeshifting"])
  ]

resolver :: Monad m => RootResolverValue e m
resolver = mkNamedResolvers
    [ ("Query", const [( "deity", mkRef "Deity" <$> getArgument "id")]),
      ("Deity", deityResolver)
    ]

api :: ByteString -> IO  ByteString
api = runApp (mkApp schema resolver)
```
