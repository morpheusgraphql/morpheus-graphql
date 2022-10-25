{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.Visitors
  ( VisitType (..),
    VisitField (..),
    VisitEnum (..),
  )
where

import Data.Morpheus.Types.Internal.AST (CONST, Value)
import Relude

class VisitType a where
  -- | Construct a new type name depending on whether it is an input,
  -- and being given the original type name.
  visitTypeName :: a -> Bool -> Text -> Text
  visitTypeName _ _ = id

  visitTypeDescription :: a -> Maybe Text -> Maybe Text
  visitTypeDescription = const id

  -- | Function applied to field labels.
  -- Handy for removing common record prefixes for example.
  visitFieldNames :: a -> Text -> Text
  visitFieldNames _ = id

  -- | Function applied to enum values
  -- Handy for removing common enum prefixes for example.
  visitEnumNames :: a -> Text -> Text
  visitEnumNames _ = id

class VisitField a where
  visitFieldName :: a -> Text -> Text
  visitFieldName _ = id

  visitFieldDescription :: a -> Maybe Text -> Maybe Text
  visitFieldDescription _ = id

  visitFieldDefaultValue :: a -> Maybe (Value CONST) -> Maybe (Value CONST)
  visitFieldDefaultValue _ = id

class VisitEnum a where
  visitEnumName :: a -> Text -> Text
  visitEnumName _ = id

  visitEnumDescription :: a -> Maybe Text -> Maybe Text
  visitEnumDescription _ = id
