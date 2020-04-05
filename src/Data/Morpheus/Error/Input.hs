{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Error.Input
  ( typeViolation )
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