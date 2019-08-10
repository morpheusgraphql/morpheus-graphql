{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Type
  ( Type(..)
  , DeprecationArgs(..)
  ) where

import           Data.Morpheus.Kind              (OBJECT)
import           Data.Morpheus.Schema.EnumValue  (EnumValue)
import qualified Data.Morpheus.Schema.Field      as F (Field (..))
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind)
import           Data.Morpheus.Types.GQLType     (GQLType (KIND, __typeName, __typeVisibility))
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

instance GQLType Type where
  type KIND Type = OBJECT
  __typeName = const "__Type"
  __typeVisibility = const False

data Type = Type
  { kind          :: TypeKind
  , name          :: Maybe Text
  , description   :: Maybe Text
  , fields        :: DeprecationArgs -> Either String (Maybe [F.Field Type])
  , interfaces    :: Maybe [Type]
  , possibleTypes :: Maybe [Type]
  , enumValues    :: DeprecationArgs -> Either String (Maybe [EnumValue])
  , inputFields   :: Maybe [I.InputValue Type]
  , ofType        :: Maybe Type
  } deriving (Generic)

newtype DeprecationArgs = DeprecationArgs
  { includeDeprecated :: Maybe Bool
  } deriving (Generic)
