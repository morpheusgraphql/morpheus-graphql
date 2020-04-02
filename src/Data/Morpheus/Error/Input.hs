{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Error.Input
  ( expectedTypeAFoundB
  , unknownField
  , undefinedField
  , Prop(..)
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
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( ResolvedValue )

data Prop =
  Prop
    { propKey  :: Text
    , propType :: Text
    }

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
  <> "Expected type \""
  <> expected <> "\" found "
  <> T.pack (unpack $ encode found)
  <> "; " <> errorMessage <> "."

undefinedField :: [Prop] -> Text -> Text
undefinedField path field =
  pathToText path <> "Undefined Field \"" <> field <> "\"."

unknownField :: [Prop] -> Text -> Text
unknownField path field =
  pathToText path <> "Unknown Field \"" <> field <> "\"."
