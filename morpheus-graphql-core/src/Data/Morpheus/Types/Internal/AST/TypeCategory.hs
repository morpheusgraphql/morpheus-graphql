{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TypeCategory
  ( TypeCategory,
    OUT,
    IN,
    ANY,
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
import Prelude (Bool (..))

data TypeCategory = In | Out | Any

type IN = 'In

type OUT = 'Out

type ANY = 'Any

class ToAny a where
  toAny :: a (k :: TypeCategory) -> a ANY

class FromAny a (k :: TypeCategory) where
  fromAny :: a ANY -> Maybe (a k)

type family IsSelected (c :: TypeCategory) (a :: TypeCategory) :: Bool

type instance IsSelected ANY a = TRUE

type instance IsSelected OUT OUT = TRUE

type instance IsSelected IN IN = TRUE

type instance IsSelected IN OUT = FALSE

type instance IsSelected OUT IN = FALSE

type instance IsSelected a ANY = TRUE
