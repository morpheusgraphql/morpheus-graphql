{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.PreProcess.Input.Enum
  ( validateEnum
  ) where

import           Data.List                           (elem)
import           Data.Morpheus.Error.Arguments       (unsupportedArgumentType)
import           Data.Morpheus.Error.Variable        (invalidEnumOption)
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.JSType          (JSType (..))
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..))
import           Data.Morpheus.Types.Query.Selection (Argument (..))
import           Data.Text                           (Text)

validateEnum :: Text -> [Text] -> Argument -> Validation Argument
validateEnum typeName' tags' (Argument (JSEnum argument) pos) =
  if argument `elem` tags'
    then pure (Argument (JSEnum argument) pos)
    else Left $ invalidEnumOption $ MetaInfo {typeName = typeName', key = argument, position = pos}
validateEnum typeName' _ (Argument _ pos) =
  Left $ unsupportedArgumentType $ MetaInfo {typeName = typeName', key = "", position = pos}
