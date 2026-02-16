{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Input.DefaultValues
  ( api,
    rootResolver,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
  ( DefaultValue (..),
    GQLRequest,
    GQLResponse,
    ID (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
  )
import Data.Text (Text, pack)

importGQLDocument "test/Feature/Input/default-values-schema.gql"

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver = Query {user, testSimple}
    }
  where
    user :: (Applicative m) => m (Maybe (User m))
    user =
      pure $
        Just $
          User
            { inputs = pure . pack . show
            }
    testSimple = pure . pack . show

-----------------------------------
api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
