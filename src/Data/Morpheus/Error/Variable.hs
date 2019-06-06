{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Variable
  ( undefinedVariable
  , unknownType
  , variableGotInvalidValue
  , uninitializedVariable
  , unusedVariables
  ) where

import           Data.Morpheus.Error.Utils               (errorMessage)
import           Data.Morpheus.Types.Internal.Base       (EnhancedKey (..), Position)
import           Data.Morpheus.Types.Internal.Validation (GQLError (..), GQLErrors)
import           Data.Text                               (Text)
import qualified Data.Text                               as T (concat)
 -- query M ( $v : String ) { a } -> "Variable \"$bla\" is never used in operation \"MyMutation\".",

{-|
VARIABLES:

Variable -> Error (position Query Head)
  data E = EN | DE
  query M ( $v : E ){...}


query Q ($a: D) ->  "Unknown type \"D\"."

case String
  - { "v" : "EN" }  ->  no error converts as enum

case type mismatch
  - { "v": { "a": "v1" ... } } -> "Variable \"$v\" got invalid value { "a": "v1" ... } ; Expected type LANGUAGE."
  - { "v" : "v1" }  -> "Variable \"$v\" got invalid value \"v1\"; Expected type LANGUAGE."
  - { "v": 1  }        "Variable \"$v\" got invalid value 1; Expected type LANGUAGE."

TODO: variable does not match to argument type
  - query M ( $v : String ) { a(p:$v) } -> "Variable \"$v\" of type \"String\" used in position expecting type \"LANGUAGE\"."
|-}
unusedVariables :: Text -> [EnhancedKey] -> GQLErrors
unusedVariables operator' = map keyToError
  where
    keyToError (EnhancedKey key' position') = GQLError {desc = text key', positions = [position']}
    text key' = T.concat ["Variable \"$", key', "\" is never used in operation \"", operator', "\"."]

variableGotInvalidValue :: Text -> Text -> Position -> GQLErrors
variableGotInvalidValue name' inputMessage' position' = errorMessage position' text
  where
    text = T.concat ["Variable \"$", name', "\" got invalid value; ", inputMessage']

unknownType :: Text -> Position -> GQLErrors
unknownType type' position' = errorMessage position' text
  where
    text = T.concat ["Unknown type \"", type', "\"."]

undefinedVariable :: Text -> Position -> Text -> GQLErrors
undefinedVariable operation' position' key' = errorMessage position' text
  where
    text = T.concat ["Variable \"", key', "\" is not defined by operation \"", operation', "\"."]

uninitializedVariable :: Position -> Text -> Text -> GQLErrors
uninitializedVariable position' type' key' = errorMessage position' text
  where
    text = T.concat ["Variable \"$", key', "\" of required type \"", type', "!\" was not provided."]
