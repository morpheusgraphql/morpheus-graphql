{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( ServerConstraint,
    httpPlayground,
    compileTimeSchemaValidation,
  )
where

import Control.Monad.IO.Unlift
  ( MonadUnliftIO,
  )
import Data.Morpheus.Server.Deriving.Schema
  ( compileTimeSchemaValidation,
  )
import Data.Morpheus.Server.Playground
  ( httpPlayground,
  )
import Relude

type ServerConstraint e m =
  ( MonadIO m,
    MonadUnliftIO m
  )
