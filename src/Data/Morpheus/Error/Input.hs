{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Error.Input
  ( undefinedField
  -- , expectedTypeAFoundB
  , typeViolation
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
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( ResolvedValue )


typeViolation :: Name -> ResolvedValue -> Message
typeViolation expected found = "Expected type \""
  <> expected
  <> "\" found "
  <> T.pack (unpack $ encode found)
  <> "."


-- expectedTypeAFoundB :: [Prop] -> Name -> ResolvedValue -> Maybe Name -> Message
-- expectedTypeAFoundB path expected found Nothing 
--   = renderPath path <> typeViolation expected found
-- expectedTypeAFoundB path expected found (Just errorMessage) =
--   renderPath path<> typeViolation expected found <> " " <> errorMessage <> "."

undefinedField :: Name -> Message
undefinedField fieldName = "Undefined Field \"" <> fieldName <> "\"."