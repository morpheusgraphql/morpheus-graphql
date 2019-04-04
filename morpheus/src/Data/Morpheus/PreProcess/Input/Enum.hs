{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.PreProcess.Input.Enum
  ( validateEnum
  ) where

import           Data.List                  (elem)
import           Data.Morpheus.Types.JSType (JSType (..))
import           Data.Text                  (Text)

validateEnum :: error -> [Text] -> JSType -> Either error JSType
validateEnum error' tags' (JSEnum enumValue) =
  if enumValue `elem` tags'
    then pure (JSEnum enumValue)
    else Left error'
validateEnum error' _ _ = Left error'
