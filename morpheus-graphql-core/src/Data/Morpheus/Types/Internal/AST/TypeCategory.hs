{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TypeCategory
  ( TypeCategory,
    OUT,
    IN,
    ANY,
    LEAF,
    OUTPUT_OBJECT,
    FromAny (..),
    ToAny (..),
    IsSelected,
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
  IsSelected
    (isType :: TypeCategory)
    (canBeType :: TypeCategory) ::
    Bool

-- ANY
type instance IsSelected ANY a = TRUE

type instance IsSelected a ANY = TRUE

-- LEAF
type instance IsSelected LEAF LEAF = TRUE

type instance IsSelected LEAF IN = TRUE

type instance IsSelected LEAF OUT = TRUE

type instance IsSelected LEAF OUTPUT_OBJECT = FALSE

-- IN
type instance IsSelected IN IN = TRUE

type instance IsSelected IN OUT = FALSE

type instance IsSelected IN OUTPUT_OBJECT = FALSE

-- OUT
type instance IsSelected OUT OUT = TRUE

type instance IsSelected OUT IN = FALSE

type instance IsSelected OUT OUTPUT_OBJECT = FALSE

-- OUTPUT_OBJECT
type instance IsSelected OUTPUT_OBJECT OUTPUT_OBJECT = TRUE

type instance IsSelected OUTPUT_OBJECT OUT = TRUE

type instance IsSelected OUTPUT_OBJECT IN = FALSE

type instance IsSelected OUTPUT_OBJECT LEAF = FALSE
