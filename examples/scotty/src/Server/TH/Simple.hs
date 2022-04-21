{-# LANGUAGE DataKinds #-}
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
  ( app,
  )
where

import Data.FileEmbed (makeRelativeToProject)
import Data.Morpheus (App, deriveApp)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
  ( Arg (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
  )
import Data.Text (Text)

importGQLDocument =<< makeRelativeToProject "src/Server/TH/simple.gql"

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver = Query {deity}
    }
  where
    deity (Arg name) =
      pure
        Deity
          { name = pure name,
            power = pure (Just "Shapeshifting")
          }

app :: App () IO
app = deriveApp rootResolver
