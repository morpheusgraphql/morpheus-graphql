{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Client.Schema
  ( defaultTypes,
  )
where

-- MORPHEUS

import Data.Morpheus.Client.QuasiQuoter (dsl)
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    FieldDefinition (..),
    FieldsDefinition,
    Name,
    TypeContent (..),
    TypeDefinition (..),
    TypeUpdater,
    TypeWrapper (..),
    createField,
    createScalarType,
    createType,
    insertType,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.Operation (Listable (..), Singleton (..))
import Data.Morpheus.Types.Internal.Resolving
  ( resolveUpdates,
  )
import Data.Text (Text)

defaultTypes :: TypeUpdater
defaultTypes = (`resolveUpdates` map insertType schemaTypes)

schemaTypes :: [TypeDefinition]
schemaTypes =
  [dsl|

  scalar Float
  scalar String
  scalar Int
  scalar Boolean

|]
