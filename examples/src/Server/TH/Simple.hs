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

module Server.TH.Simple
  ( thSimpleApi,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (GQLRootResolver (..), Undefined (..))
import Data.Text (Text)

importGQLDocument "src/Server/TH/simple.gql"

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
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

thSimpleApi :: ByteString -> IO ByteString
thSimpleApi = interpreter rootResolver
