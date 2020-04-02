{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Error.Input
  ( expectedTypeAFoundB
  , undefinedField
  , Prop(..)
  )
where

import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy.Char8     ( unpack )
import qualified Data.Text                     as T
                                                ( pack )


-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST.Base 
                                                ( Name
                                                , Message
                                                , Prop(..)
                                                , renderPath
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( ResolvedValue )

expectedTypeAFoundB :: [Prop] -> Name -> ResolvedValue -> Maybe Name -> Message
expectedTypeAFoundB path expected found Nothing 
  = renderPath path 
  <> "Expected type \""
  <> expected
  <> "\" found "
  <> T.pack (unpack $ encode found)
  <> "."

expectedTypeAFoundB path expected found (Just errorMessage) =
  renderPath path
  <> "Expected type \""
  <> expected <> "\" found "
  <> T.pack (unpack $ encode found)
  <> "; " <> errorMessage <> "."

undefinedField :: [Prop] -> Name -> Message
undefinedField path field =
  renderPath path <> "Undefined Field \"" <> field <> "\"."