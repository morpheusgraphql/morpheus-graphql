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

module API (api) where

import Control.Monad.Freer (Eff, Member)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.FileEmbed (makeRelativeToProject)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
  ( Arg (Arg),
    Deprecated (..),
    Describe (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
    liftEither,
  )
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified DeityRepo as DR
  ( DeityRepo,
    Error,
    createDeity,
    getDeityByName,
  )
import qualified Types as T
import Prelude hiding (error)

importGQLDocument =<< makeRelativeToProject "src/api.gql"

api :: (Member DR.DeityRepo effs, Typeable effs) => ByteString -> Eff effs ByteString
api = interpreter rootResolver

rootResolver :: (Member DR.DeityRepo effs) => RootResolver (Eff effs) () Query Mutation Undefined
rootResolver =
  defaultRootResolver
    { queryResolver = Query {deity = deityResolver},
      mutationResolver = Mutation {createDeity = createDeityResolver}
    }
  where
    deityResolver (Arg name) =
      liftEither $ toResponse <$> DR.getDeityByName name

    createDeityResolver CreateDeityArgs {name, power} =
      liftEither $ toResponse <$> DR.createDeity (T.Deity name power)

toResponse ::
  (Applicative m) =>
  Either DR.Error T.Deity ->
  Either String (Deity m)
toResponse (Right deity) = Right $ toResponse' deity
toResponse (Left error) = Left $ show error

toResponse' :: (Applicative m) => T.Deity -> Deity m
toResponse' (T.Deity name power) = Deity {name = pure name, power = pure power}
