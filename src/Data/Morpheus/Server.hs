{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( httpPlayground,
    compileTimeSchemaValidation,
  )
where

import Data.Morpheus.Server.Deriving.Schema
  ( compileTimeSchemaValidation,
  )
import Data.Morpheus.Server.Playground
  ( httpPlayground,
  )
