{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.PreProcess.Input.Enum
  ( validateEnum
  ) where

import           Data.Morpheus.Error.Arguments       (unsupportedArgumentType)
import           Data.Morpheus.Error.Variable        (invalidEnumOption)
import           Data.Morpheus.Schema.EnumValue      (isEnumOf)
import qualified Data.Morpheus.Schema.Type           as T (enumValues, name)
import           Data.Morpheus.Schema.Utils.Utils    (Type)
import           Data.Morpheus.Types.Describer       (unpackDeprecation)
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.JSType          (JSType (..))
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..))
import           Data.Morpheus.Types.Query.Selection (Argument (..))

validateEnum :: Type -> Argument -> Validation Argument
validateEnum _type (Argument (JSEnum argument) pos) =
  if isEnumOf argument (unpackDeprecation $ T.enumValues _type)
    then pure (Argument (JSEnum argument) pos)
    else Left $ invalidEnumOption $ MetaInfo {typeName = T.name _type, key = argument, position = pos}
validateEnum _type (Argument _ pos) =
  Left $ unsupportedArgumentType $ MetaInfo {typeName = T.name _type, key = "", position = pos}
