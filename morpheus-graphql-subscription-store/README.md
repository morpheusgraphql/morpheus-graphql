# Morpheus GraphQL Client

## Morpheus `GraphQL Client` with Template haskell QuasiQuotes

```hs
defineByDocumentFile
    "./schema.gql"
  [gql|
    query GetHero ($character: Character)
      {
        deity (fatherOf:$character) {
          name
          power
          worships {
            deity2Name: name
          }
        }
      }
  |]
```

with schema:

```gql
input Character {
  name: String!
}

type Deity {
  name: String!
  worships: Deity
  power: Power!
}

enum Power {
  Lightning
  Teleportation
  Omniscience
}
```

will validate query and Generate:

- namespaced response and variable types
- instance for `Fetch` typeClass

```hs
data GetHero = GetHero {
  deity: DeityDeity
}

-- from: {user
data DeityDeity = DeityDeity {
  name: Text,
  worships: Maybe DeityWorshipsDeity
  power: Power
}

-- from: {deity{worships
data DeityWorshipsDeity = DeityWorshipsDeity {
  name: Text,
}

data Power =
    PowerLightning
  | PowerTeleportation
  | PowerOmniscience

data GetHeroArgs = GetHeroArgs {
  character: Character
}

data Character = Character {
  name: Person
}
```

as you see, response type field name collision can be handled with GraphQL `alias`.

with `fetch` you can fetch well typed response `GetHero`.

```haskell
  fetchHero :: Args GetHero -> m (Either String GetHero)
  fetchHero = fetch jsonRes args
      where
        args = GetHeroArgs {character = Person {name = "Zeus"}}
        jsonRes :: ByteString -> m ByteString
        jsonRes = <GraphQL APi>
```

in this case, `jsonRes` resolves a request into a response in some monad `m`.

A `fetch` resolver implementation against [a real API](https://swapi.graph.cool) may look like the following:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Req

resolver :: String -> ByteString -> IO ByteString
resolver tok b = runReq defaultHttpConfig $ do
    let headers = header "Content-Type" "application/json"
    responseBody <$> req POST (https "swapi.graph.cool") (ReqBodyLbs b) lbsResponse headers
```

this is demonstrated in examples/src/Client/StarWarsClient.hs

types can be generated from `introspection` too:

```haskell
defineByIntrospectionFile "./introspection.json"
```
