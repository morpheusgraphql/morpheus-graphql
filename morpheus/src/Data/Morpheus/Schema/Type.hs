{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Morpheus.Schema.Type
  ( Type(..)
  , DeprecationArgs(..)
  ) where

import           Data.Data                       (Data)
import           Data.Morpheus.Schema.EnumValue  (EnumValue)
import qualified Data.Morpheus.Schema.Field      as F (Field (..))
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind)
import           Data.Morpheus.Types.Describer   (Deprecation (..), EnumOf)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

data Type = Type
  { kind          :: EnumOf TypeKind
  , name          :: Text
  , description   :: Text
  , fields        :: Deprecation [F.Field Type]
  , ofType        :: Maybe Type
  , interfaces    :: [Type]
  , possibleTypes :: [Type]
  , enumValues    :: Deprecation [EnumValue]
  , inputFields   :: [I.InputValue Type]
  } deriving (Show, Data, Generic)

newtype DeprecationArgs = DeprecationArgs
  { includeDeprecated :: Maybe Bool
  } deriving (Show, Data, Generic)
