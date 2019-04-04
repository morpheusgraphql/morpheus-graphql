{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.PreProcess.Input.Enum
  ( validateEnum
  , validateEnumArgument
  ) where

import           Data.List                           (elem)
import           Data.Morpheus.Error.Input           (expectedTypeAFoundB)
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.JSType          (JSType (..))
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..))
import           Data.Morpheus.Types.Query.Selection (Argument (..))
import           Data.Text                           (Text)

validateEnumArgument :: Text -> [Text] -> Argument -> Validation Argument
validateEnumArgument typeName' tags' (Argument jsValue pos) = (`Argument` pos) <$> validateEnum error' tags' jsValue
  where
    error' = expectedTypeAFoundB meta' jsValue
    meta' = MetaInfo {typeName = typeName', key = "TODO: Argument", position = pos}

validateEnum :: error -> [Text] -> JSType -> Either error JSType
validateEnum error' tags' (JSEnum enumValue) =
  if enumValue `elem` tags'
    then pure (JSEnum enumValue)
    else Left error'
validateEnum error' _ _ = Left error'
