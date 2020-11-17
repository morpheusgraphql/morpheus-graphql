{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TypeCategory
  ( TypeCategory,
    ELEM,
    OUT,
    IN,
    ANY,
    LEAF,
    OBJECT,
    IMPLEMENTABLE,
    fromAny,
    toAny,
    REQUIRE_IMPLEMENTABLE,
    ToCategory (..),
    FromCategory (..),
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( FALSE,
    TRUE,
  )
import Data.Morpheus.Types.Internal.AST.Stage (Stage)
import Relude

data TypeCategory
  = IN
  | OUT
  | ANY
  | LEAF
  | OBJECT
  | IMPLEMENTABLE

type IN = 'IN

type OUT = 'OUT

type ANY = 'ANY

type OBJECT = 'OBJECT

type IMPLEMENTABLE = 'IMPLEMENTABLE

type LEAF = 'LEAF

toAny ::
  (ToCategory a k ANY) =>
  a (k :: TypeCategory) (s :: Stage) ->
  a ANY s
toAny = toCategory

fromAny ::
  (FromCategory a ANY k) =>
  a ANY (s :: Stage) ->
  Maybe (a k s)
fromAny = fromCategory

class ToCategory a (k :: TypeCategory) (k' :: TypeCategory) where
  toCategory :: a k (s :: Stage) -> a k' s

class FromCategory a (k :: TypeCategory) (k' :: TypeCategory) where
  fromCategory :: a k (s :: Stage) -> Maybe (a k' s)

type family ELEM (elemKind :: TypeCategory) (setOfKind :: TypeCategory) :: Bool where
-- same types
  ELEM a a = TRUE
-- any
  ELEM ANY a = TRUE
  ELEM a ANY = FALSE
-- leaf
  ELEM LEAF IN = TRUE
  ELEM LEAF OUT = TRUE
-- implementable
  ELEM IMPLEMENTABLE OUT = TRUE
-- object
  ELEM OBJECT IMPLEMENTABLE = TRUE
  ELEM OBJECT OUT = TRUE
-- all other cases are false
  ELEM a b = FALSE

type REQUIRE_IMPLEMENTABLE cat = ELEM cat IMPLEMENTABLE ~ TRUE
