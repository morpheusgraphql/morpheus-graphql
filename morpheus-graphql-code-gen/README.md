# Morpheus GraphQL Code Gen

Morpheus GraphQL Code Gen helps you to generate GraphQL APIs .

Morpheus GraphQL CLI is still in an early stage of development, so any feedback is more than welcome, and we appreciate any contribution!
Just open an issue here on GitHub, or join [our Slack channel](https://morpheus-graphql-slack-invite.herokuapp.com/) to get in touch.

## Getting Started

Generating dummy Morpheus Api from `schema.gql`

```ssh
morpheus build api/*.gql --root api
```

_src/schema.gql_

```gql
type Query {
  deity(name: String!): Deity
  deities: [Deity!]!
}

"""
deity description
"""
type Deity {
  """
  name description
  """
  name: String!
  power: String
}
```

_src/Schema.hs_

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

import Data.Data (Typeable)
import Data.Map (empty, fromList)
import Data.Morpheus.Kind (TYPE)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)

---- GQL Query -------------------------------
data Query m = Query
  { queryDeity :: Arg "name" Text -> m (Maybe (Deity m)),
    queryDeities :: m [Deity m]
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE

---- GQL Deity -------------------------------
data Deity m = Deity
  { deityName :: m Text,
    deityPower :: m (Maybe Text)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Deity m) where
  type KIND (Deity m) = TYPE
  description _ = Just "\ndeity description\n"
  getDescriptions _ = fromList [("name", "\n  name description\n  ")]
```
