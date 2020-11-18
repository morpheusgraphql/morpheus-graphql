{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TypeCategory
  ( TypeCategory,
    type (<=?),
    type (<=!),
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

type (a :: TypeCategory) <=? (b :: TypeCategory) = a <=! b ~ TRUE

-- <=
type family (elem :: TypeCategory) <=! (cat :: TypeCategory) :: Bool where
-- leaf
  LEAF <=! IN = TRUE
  LEAF <=! OUT = TRUE
-- object
  OBJECT <=! IMPLEMENTABLE = TRUE
  OBJECT <=! OUT = TRUE
-- implementable
  IMPLEMENTABLE <=! OUT = TRUE
-- all other cases are false
  ANY <=! a = TRUE
  a <=! ANY = TRUE
  a <=! a = TRUE
  a <=! b = FALSE

type REQUIRE_IMPLEMENTABLE cat = cat <=? IMPLEMENTABLE
