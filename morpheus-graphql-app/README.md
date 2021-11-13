# Morpheus GraphQL App

provides utilities for creating executable GraphQL applications for servers. You can use it to create a schema-first GraphQL server with dynamic typings.

## Build schema-first GraphQL App with dynamic typings

###### schema.gql

```gql
type Deity {
  name: String
  power: [String!]
}

type Query {
  deity(id: ID): Deity
}
```

###### App.hs

```hs
deityResolver :: Monad m => NamedResolverFunction QUERY e m
deityResolver arg =
  object
    [ ("name", pure "Morpheus"),
      ("power", pure $ list [enum "Shapeshifting"])
    ]

resolver :: Monad m => RootResolverValue e m
resolver =
  queryResolvers
    [ ( "Query", const $ object [("deity", ref "Deity" <$> getArgument "id")]),
      ("Deity", deityResolver)
    ]

api :: ByteString -> IO  ByteString
api query = do
  schema <- LBS.readFile "./schema.gql" >>= resultOr (fail . show) pure . parseSchema
  runApp (mkApp schema resolver) query
```
