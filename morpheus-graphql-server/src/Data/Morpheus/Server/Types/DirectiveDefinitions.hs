{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.DirectiveDefinitions
  ( Prefixes (..),
    Suffixes (..),
    Deprecated (..),
    Describe (..),
    Rename (..),
    DropNamespace (..),
    DefaultValue (..),
  )
where

import Data.Morpheus.Server.Types.Directives (GQLDirective (..))
import Data.Morpheus.Server.Types.GQLType (GQLType (..))
import Data.Morpheus.Server.Types.Internal
  ( mkTypeData,
    stripConstructorNamespace,
    stripFieldNamespace,
  )
import Data.Morpheus.Server.Types.Kind (DIRECTIVE)
import Data.Morpheus.Server.Types.Visitors
  ( VisitEnum (..),
    VisitField (..),
    VisitType (..),
  )
import Data.Morpheus.Types.Internal.AST (CONST, DirectiveLocation (..), Value)
import Data.Text (take, drop, length, pack, unpack)
import Relude hiding (take, drop, length)

-- | a custom GraphQL directive for adding or removing
-- of prefixes
data Prefixes = Prefixes
  { addPrefix :: Text,
    removePrefix :: Text
  }
  deriving (Generic)

instance GQLType Prefixes where
  type KIND Prefixes = DIRECTIVE

instance GQLDirective Prefixes where
  type
    DIRECTIVE_LOCATIONS Prefixes =
      '[ 'LOCATION_OBJECT,
         'LOCATION_ENUM,
         'LOCATION_INPUT_OBJECT,
         'LOCATION_UNION,
         'LOCATION_SCALAR,
         'LOCATION_INTERFACE
       ]

instance VisitType Prefixes where
  visitTypeName Prefixes {addPrefix, removePrefix} _ name = addPrefix <> drop (length removePrefix) name
  visitTypeDescription _ = id

-- native GraphQL directive @deprecated
--
newtype Deprecated = Deprecated
  { reason :: Maybe Text
  }
  deriving (Generic)
  deriving anyclass
    ( VisitEnum,
      VisitField
    )

instance GQLType Deprecated where
  type KIND Deprecated = DIRECTIVE
  __type = mkTypeData "deprecated"

instance GQLDirective Deprecated where
  type
    DIRECTIVE_LOCATIONS Deprecated =
      '[ 'LOCATION_FIELD_DEFINITION,
         'LOCATION_ENUM_VALUE
       ]

newtype Describe = Describe {text :: Text}
  deriving
    ( Generic
    )

instance GQLType Describe where
  type KIND Describe = DIRECTIVE

instance GQLDirective Describe where
  type
    DIRECTIVE_LOCATIONS Describe =
      '[ 'LOCATION_ENUM_VALUE,
         'LOCATION_FIELD_DEFINITION,
         'LOCATION_INPUT_FIELD_DEFINITION,
         'LOCATION_OBJECT,
         'LOCATION_ENUM,
         'LOCATION_INPUT_OBJECT,
         'LOCATION_UNION,
         'LOCATION_SCALAR,
         'LOCATION_INTERFACE,
         'LOCATION_ARGUMENT_DEFINITION
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
    ( Generic
    )

instance GQLType Rename where
  type KIND Rename = DIRECTIVE

instance GQLDirective Rename where
  excludeFromSchema _ = True
  type
    DIRECTIVE_LOCATIONS Rename =
      '[ 'LOCATION_OBJECT,
         'LOCATION_ENUM,
         'LOCATION_INPUT_OBJECT,
         'LOCATION_UNION,
         'LOCATION_SCALAR,
         'LOCATION_INTERFACE,
         'LOCATION_ENUM_VALUE,
         'LOCATION_FIELD_DEFINITION,
         'LOCATION_INPUT_FIELD_DEFINITION
       ]

instance VisitType Rename where
  visitTypeName Rename {newName} _ _ = newName
  visitTypeDescription _ = id

instance VisitEnum Rename where
  visitEnumName Rename {newName} _ = newName

instance VisitField Rename where
  visitFieldName Rename {newName} _ = newName

-- DropTypeNamespace
newtype DropNamespace = DropNamespace
  { dropNamespace :: Text
  }
  deriving
    ( Generic
    )

instance GQLType DropNamespace where
  type KIND DropNamespace = DIRECTIVE

instance GQLDirective DropNamespace where
  type
    DIRECTIVE_LOCATIONS DropNamespace =
      '[ 'LOCATION_OBJECT,
         'LOCATION_ENUM,
         'LOCATION_INPUT_OBJECT,
         'LOCATION_UNION,
         'LOCATION_SCALAR,
         'LOCATION_INTERFACE
       ]
  excludeFromSchema _ = True

instance VisitType DropNamespace where
  visitFieldNames DropNamespace {dropNamespace} = pack . stripFieldNamespace dropNamespace . unpack
  visitEnumNames DropNamespace {dropNamespace} = pack . stripConstructorNamespace dropNamespace . unpack

newtype DefaultValue = DefaultValue
  { defaultValue :: Value CONST
  }
  deriving (Generic)

instance GQLType DefaultValue where
  type KIND DefaultValue = DIRECTIVE

instance GQLDirective DefaultValue where
  type DIRECTIVE_LOCATIONS DefaultValue = '[ 'LOCATION_INPUT_FIELD_DEFINITION]
  excludeFromSchema _ = True

instance VisitField DefaultValue where
  visitFieldDefaultValue DefaultValue {defaultValue} _ = Just defaultValue

-- | a custom GraphQL directive for adding or removing
-- of suffixes
data Suffixes = Suffixes
  { addSuffix :: Text,
    removeSuffix :: Text
  } 
  deriving (Generic)

instance GQLType Suffixes where
  type KIND Suffixes = DIRECTIVE

instance GQLDirective Suffixes where
  type
    DIRECTIVE_LOCATIONS Suffixes =
      '[ 'LOCATION_OBJECT,
         'LOCATION_ENUM,
         'LOCATION_INPUT_OBJECT,
         'LOCATION_UNION,
         'LOCATION_SCALAR,
         'LOCATION_INTERFACE
       ]

instance VisitType Suffixes where
  visitTypeName Suffixes {addSuffix, removeSuffix} _ name = 
    take (length name - length removeSuffix) name <> addSuffix
  visitTypeDescription _ = id