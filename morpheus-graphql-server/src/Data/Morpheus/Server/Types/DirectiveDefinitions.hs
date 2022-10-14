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
    Rename (..),
    DropNamespace (..),
  )
where

import Data.Morpheus.Server.Types.Directives (GQLDirective (..))
import Data.Morpheus.Server.Types.GQLType (GQLType (__type))
import Data.Morpheus.Server.Types.Internal
  ( mkTypeData,
    stripConstructorNamespace,
    stripFieldNamespace,
  )
import Data.Morpheus.Server.Types.Visitors
  ( VisitEnum (..),
    VisitField (..),
    VisitType (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation (..),
  )
import Data.Text (drop, length, pack, unpack)
import Relude hiding (drop, length)

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
  visitTypeName Prefixes {addPrefix, removePrefix} _ name = addPrefix <> drop (length removePrefix) name
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

-- description

newtype Describe = Describe {text :: Text}
  deriving
    ( GQLType,
      Generic
    )

instance GQLDirective Describe where
  type
    DIRECTIVE_LOCATIONS Describe =
      '[ 'ENUM_VALUE,
         'FIELD_DEFINITION,
         'INPUT_FIELD_DEFINITION,
         'OBJECT,
         'ENUM,
         'INPUT_OBJECT,
         'UNION,
         'SCALAR,
         'INTERFACE,
         'ARGUMENT_DEFINITION
       ]

instance VisitEnum Describe where
  visitEnumDescription Describe {text} _ = Just text

instance VisitField Describe where
  visitFieldDescription Describe {text} _ = Just text

instance VisitType Describe where
  visitTypeDescription Describe {text} _ = Just text

-- | a custom GraphQL directive for adding or removing
-- of prefixes
newtype Rename = Rename {newName :: Text}
  deriving
    ( Generic,
      GQLType
    )

instance GQLDirective Rename where
  type
    DIRECTIVE_LOCATIONS Rename =
      '[ 'OBJECT,
         'ENUM,
         'INPUT_OBJECT,
         'UNION,
         'SCALAR,
         'INTERFACE,
         'ENUM_VALUE,
         'FIELD_DEFINITION,
         'INPUT_FIELD_DEFINITION
       ]

instance VisitType Rename where
  visitTypeName Rename {newName} _ _ = newName
  visitTypeDescription _ = id

instance VisitEnum Rename where
  visitEnumName Rename {newName} _ = newName

instance VisitField Rename where
  visitFieldName Rename {newName} _ = newName

-- DropTypeNamespace
newtype DropNamespace = DropNamespace {dropNamespace :: Text}
  deriving
    ( Generic,
      GQLType
    )

instance GQLDirective DropNamespace where
  type
    DIRECTIVE_LOCATIONS DropNamespace =
      '[ 'OBJECT,
         'ENUM,
         'INPUT_OBJECT,
         'UNION,
         'SCALAR,
         'INTERFACE
       ]

instance VisitType DropNamespace where
  visitFieldNames DropNamespace {dropNamespace} = pack . stripFieldNamespace dropNamespace . unpack
  visitEnumNames DropNamespace {dropNamespace} = pack . stripConstructorNamespace dropNamespace . unpack
