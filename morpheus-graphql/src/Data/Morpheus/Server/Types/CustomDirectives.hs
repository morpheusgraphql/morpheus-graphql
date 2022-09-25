{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Server.Types.CustomDirectives (DirectivePrefix (..)) where

import Data.Morpheus.App
import Data.Morpheus.Server.Types.Directives
import Data.Morpheus.Server.Types.GQLType
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation (..),
    packName,
  )
import Data.Text
import GHC.Generics (Generic)

data DirectivePrefix = DirectivePrefix
  { prefix :: Text,
    drop :: Bool
  }
  deriving (Generic, GQLType)

prefixName :: Text -> VisitorTypeDefinition -> VisitorTypeDefinition
prefixName prefix t = t {visitorTypeName = packName prefix <> visitorTypeName t}

instance GQLDirective DirectivePrefix where
  type ALLOWED_DIRECTIVE_LOCATIONS DirectivePrefix = '[ 'OBJECT, 'ENUM, 'INPUT_OBJECT]

  visit DirectivePrefix {prefix} (VisitEnum t) = pure $ VisitEnum $ prefixName prefix t
  visit DirectivePrefix {prefix} (VisitInputObject t) = pure $ VisitInputObject $ prefixName prefix t
  visit DirectivePrefix {prefix} (VisitObject t) = pure $ VisitObject $ prefixName prefix t
