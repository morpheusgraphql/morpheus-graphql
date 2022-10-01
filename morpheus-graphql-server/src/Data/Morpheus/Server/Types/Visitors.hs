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

import Relude

class VisitType a where
  visitTypeName :: a -> Text -> Text
  visitTypeName _ = id

  visitTypeDescription :: a -> Maybe Text -> Maybe Text
  visitTypeDescription = const id

class VisitField a where
  visitFieldName :: a -> Text -> Text
  visitFieldName _ = id

  visitFieldDescription :: a -> Maybe Text -> Maybe Text
  visitFieldDescription _ = id

class VisitEnum a where
  visitEnumName :: a -> Text -> Text
  visitEnumName _ = id

  visitEnumDescription :: a -> Maybe Text -> Maybe Text
  visitEnumDescription _ = id
