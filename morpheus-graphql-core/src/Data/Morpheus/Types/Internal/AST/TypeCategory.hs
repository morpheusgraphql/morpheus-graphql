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

type family
  ELEM
    (elemKind :: TypeCategory)
    (setOfKind :: TypeCategory) ::
    Bool

type REQUIRE_IMPLEMENTABLE cat = ELEM cat IMPLEMENTABLE ~ TRUE

-- ANY
type instance ELEM ANY a = TRUE

type instance ELEM a ANY = TRUE

-- LEAF
type instance ELEM LEAF LEAF = TRUE

type instance ELEM LEAF IN = TRUE

type instance ELEM LEAF OUT = TRUE

type instance ELEM LEAF OBJECT = FALSE

type instance ELEM LEAF IMPLEMENTABLE = FALSE

-- IN
type instance ELEM IN IN = TRUE

type instance ELEM IN OUT = FALSE

type instance ELEM IN OBJECT = FALSE

type instance ELEM IN IMPLEMENTABLE = FALSE

-- OUT
type instance ELEM OUT OUT = TRUE

type instance ELEM OUT IN = FALSE

type instance ELEM OUT OBJECT = FALSE

type instance ELEM OUT IMPLEMENTABLE = FALSE

-- IMPLEMENTABLE
type instance ELEM IMPLEMENTABLE IMPLEMENTABLE = TRUE

type instance ELEM IMPLEMENTABLE OUT = TRUE

type instance ELEM IMPLEMENTABLE IN = FALSE

type instance ELEM IMPLEMENTABLE LEAF = FALSE

type instance ELEM IMPLEMENTABLE OBJECT = FALSE

-- OUTPUT_OBJECT
type instance ELEM OBJECT OBJECT = TRUE

type instance ELEM OBJECT IMPLEMENTABLE = TRUE

type instance ELEM OBJECT OUT = TRUE

type instance ELEM OBJECT IN = FALSE

type instance ELEM OBJECT LEAF = FALSE
