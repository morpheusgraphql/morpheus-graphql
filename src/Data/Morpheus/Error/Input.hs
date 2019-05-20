{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Input
  ( inputErrorMessage
  , InputError(..)
  , Prop(..)
  , InputValidation
  ) where

import           Data.Aeson                         (encode)
import           Data.ByteString.Lazy.Char8         (unpack)
import           Data.Morpheus.Types.Internal.Value (Value)
import           Data.Text                          (Text)
import qualified Data.Text                          as T (concat, intercalate, pack)

type InputValidation a = Either InputError a

data InputError
  = UnexpectedType [Prop]
                   Text
                   Value
  | UndefinedField [Prop]
                   Text
  | UnknownField [Prop]
                 Text

data Prop = Prop
  { propKey  :: Text
  , propType :: Text
  }

inputErrorMessage :: InputError -> Text
inputErrorMessage (UnexpectedType path type' jsType) = expectedTypeAFoundB path type' jsType
inputErrorMessage (UndefinedField path' field')      = undefinedField path' field'
inputErrorMessage (UnknownField path' field')        = unknownField path' field'

pathToText :: [Prop] -> Text
pathToText []    = ""
pathToText path' = T.concat ["on ", T.intercalate "." $ fmap propKey path']

expectedTypeAFoundB :: [Prop] -> Text -> Value -> Text
expectedTypeAFoundB path' expected found =
  T.concat [pathToText path', " Expected type \"", expected, "\" found ", T.pack (unpack $ encode found), "."]

undefinedField :: [Prop] -> Text -> Text
undefinedField path' field' = T.concat [pathToText path', " Undefined Field \"", field', "\"."]

unknownField :: [Prop] -> Text -> Text
unknownField path' field' = T.concat [pathToText path', " Unknown Field \"", field', "\"."]
