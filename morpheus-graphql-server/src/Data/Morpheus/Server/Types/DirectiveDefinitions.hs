{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.DirectiveDefinitions
  ( Prefixes (..),
    Deprecated (..),
    Describe (..),
  )
where

import Data.Morpheus.Server.Types.Directives (GQLDirective (..))
import Data.Morpheus.Server.Types.GQLType (GQLType (__type))
import Data.Morpheus.Server.Types.Internal (mkTypeData)
import Data.Morpheus.Server.Types.Visitors
  ( VisitEnum (..),
    VisitField,
    VisitType (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation (..),
  )
import qualified Data.Text as T
import Relude

-- | a custom GraphQL directive for adding or removing
-- of prefixes
data Prefixes = Prefixes
  { addPrefix :: Text,
    removePrefix :: Text
  }
  deriving (Generic, GQLType)

instance GQLDirective Prefixes where
  type
    DIRECTIVE_LOCATIONS Prefixes =
      '[ 'OBJECT,
         'ENUM,
         'INPUT_OBJECT,
         'UNION,
         'SCALAR,
         'INTERFACE
       ]

instance VisitType Prefixes where
  visitTypeName Prefixes {addPrefix, removePrefix} name = addPrefix <> T.drop (T.length removePrefix) name
  visitTypeDescription _ = id

-- native GraphQL directive @deprecated
--
newtype Deprecated = Deprecated
  { reason :: Maybe Text
  }
  deriving
    ( Generic,
      VisitEnum,
      VisitField
    )

instance GQLType Deprecated where
  __type _ = mkTypeData "deprecated"

instance GQLDirective Deprecated where
  type
    DIRECTIVE_LOCATIONS Deprecated =
      '[ 'FIELD_DEFINITION,
         'ENUM_VALUE
       ]

newtype Describe = Describe {text :: Text}
  deriving
    ( GQLType,
      Generic
    )

instance GQLDirective Describe where
  type DIRECTIVE_LOCATIONS Describe = '[ 'ENUM_VALUE]

instance VisitEnum Describe where
  visitEnumDescription Describe {text} _ = Just text
