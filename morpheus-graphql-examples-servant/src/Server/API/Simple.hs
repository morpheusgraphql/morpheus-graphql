{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Server.API.Simple
  ( api,
    apiPubSub,
    rootResolver,
    EVENT,
  )
where

import Data.Morpheus
  ( interpreter,
  )
import Data.Morpheus.Document
  ( importGQLDocument,
  )
import Data.Morpheus.Types
  ( Event (..),
    GQLRequest,
    GQLResponse,
    Input,
    RootResolver (..),
    Stream,
    Undefined (..),
  )
import Data.Text (Text)

importGQLDocument "src/Server/API/simple.gql"

type EVENT = Event Label Contet

data Label
  = Update
  | New
  deriving (Eq, Show)

newtype Contet = Contet {id :: Text}

rootResolver :: RootResolver IO EVENT Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    deity DeityArgs {name} =
      pure
        Deity
          { name = pure name,
            power = pure (Just "Shapeshifting")
          }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver

apiPubSub :: Input api -> Stream api EVENT IO
apiPubSub = interpreter rootResolver
