{-# LANGUAGE DataKinds #-}
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
    OUTPUT_OBJECT,
    FromAny (..),
    ToAny (..),
  )
where

import Data.Maybe (Maybe (..))
import Data.Morpheus.Types.Internal.AST.Base
  ( FALSE,
    TRUE,
  )
import Data.Morpheus.Types.Internal.AST.Stage (Stage)
import Prelude (Bool (..))

data TypeCategory
  = IN
  | OUT
  | ANY
  | LEAF
  | OUTPUT_OBJECT

type IN = 'IN

type OUT = 'OUT

type ANY = 'ANY

type OUTPUT_OBJECT = 'OUTPUT_OBJECT

type LEAF = 'LEAF

class ToAny a where
  toAny :: a (k :: TypeCategory) (s :: Stage) -> a ANY s

class FromAny a (k :: TypeCategory) where
  fromAny :: a ANY (s :: Stage) -> Maybe (a k s)

type family
  ELEM
    (elemKind :: TypeCategory)
    (setOfKind :: TypeCategory) ::
    Bool

-- ANY
type instance ELEM ANY a = TRUE

type instance ELEM a ANY = TRUE

-- LEAF
type instance ELEM LEAF LEAF = TRUE

type instance ELEM LEAF IN = TRUE

type instance ELEM LEAF OUT = TRUE

type instance ELEM LEAF OUTPUT_OBJECT = FALSE

-- IN
type instance ELEM IN IN = TRUE

type instance ELEM IN OUT = FALSE

type instance ELEM IN OUTPUT_OBJECT = FALSE

-- OUT
type instance ELEM OUT OUT = TRUE

type instance ELEM OUT IN = FALSE

type instance ELEM OUT OUTPUT_OBJECT = FALSE

-- OUTPUT_OBJECT
type instance ELEM OUTPUT_OBJECT OUTPUT_OBJECT = TRUE

type instance ELEM OUTPUT_OBJECT OUT = TRUE

type instance ELEM OUTPUT_OBJECT IN = FALSE

type instance ELEM OUTPUT_OBJECT LEAF = FALSE
