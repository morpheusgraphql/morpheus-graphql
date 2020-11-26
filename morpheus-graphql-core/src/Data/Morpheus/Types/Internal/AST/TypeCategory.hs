{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TypeCategory
  ( TypeCategory (IN, OUT),
    type (<=!),
    type (<=?),
    OUT,
    IN,
    ANY,
    LEAF,
    OBJECT,
    INPUT_OBJECT,
    IMPLEMENTABLE,
    fromAny,
    toAny,
    ToCategory (..),
    FromCategory (..),
    ToOBJECT,
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
  | INPUT_OBJECT
  | IMPLEMENTABLE
  deriving (Show, Eq, Ord)

type IN = 'IN

type OUT = 'OUT

type ANY = 'ANY

type OBJECT = 'OBJECT

type IMPLEMENTABLE = 'IMPLEMENTABLE

type LEAF = 'LEAF

type INPUT_OBJECT = 'INPUT_OBJECT

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

type (a :: TypeCategory) <=! (b :: TypeCategory) = a <=? b ~ TRUE

-- <=
type family (elem :: TypeCategory) <=? (cat :: TypeCategory) :: Bool where
-- leaf
  LEAF <=? IN = TRUE
  LEAF <=? OUT = TRUE
-- input
  INPUT_OBJECT <=? IN = TRUE
-- output
  OBJECT <=? IMPLEMENTABLE = TRUE
  OBJECT <=? OUT = TRUE
  IMPLEMENTABLE <=? OUT = TRUE
-- all other cases are false
  ANY <=? a = TRUE
  a <=? ANY = TRUE
  a <=? a = TRUE
  a <=? b = FALSE

type family ToOBJECT (s :: TypeCategory) :: TypeCategory where
  ToOBJECT OUT = OBJECT
  ToOBJECT IN = INPUT_OBJECT
