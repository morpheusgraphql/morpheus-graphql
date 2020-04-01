{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Error.Input
  ( inputErrorMessage
  , InputError(..)
  , Prop(..)
  , InputValidation
  )
where

import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
                                                ( intercalate
                                                , pack
                                                )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( GQLErrors )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( ResolvedValue )

type InputValidation a = Either InputError a

data InputError
  = UnexpectedType [Prop] Text ResolvedValue (Maybe Text)
  | UndefinedField [Prop] Text
  | UnknownField [Prop] Text
  | GlobalInputError GQLErrors

data Prop =
  Prop
    { propKey  :: Text
    , propType :: Text
    }

inputErrorMessage :: InputError -> Either GQLErrors Text
inputErrorMessage (UnexpectedType path type' value errorMessage) =
  Right $ expectedTypeAFoundB path type' value errorMessage
inputErrorMessage (UndefinedField path' field') =
  Right $ undefinedField path' field'
inputErrorMessage (UnknownField path' field') =
  Right $ unknownField path' field'
inputErrorMessage (GlobalInputError err) = Left err

pathToText :: [Prop] -> Text
pathToText []    = ""
pathToText path = "on " <> T.intercalate "." (fmap propKey path)

expectedTypeAFoundB :: [Prop] -> Text -> ResolvedValue -> Maybe Text -> Text
expectedTypeAFoundB path expected found Nothing 
  = pathToText path 
  <> " Expected type \""
  <> expected
  <> "\" found "
  <> T.pack (unpack $ encode found)
  <> "."

expectedTypeAFoundB path expected found (Just errorMessage) =
  pathToText path
  <> " Expected type \""
  <> expected <> "\" found "
  <> T.pack (unpack $ encode found)
  <> "; " <> errorMessage <> "."

undefinedField :: [Prop] -> Text -> Text
undefinedField path field =
  pathToText path <> " Undefined Field \"" <> field <> "\"."

unknownField :: [Prop] -> Text -> Text
unknownField path field =
  pathToText path <> " Unknown Field \"" <> field <> "\"."
