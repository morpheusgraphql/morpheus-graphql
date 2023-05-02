{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.Proxy
  ( selNameProxy,
    ContextValue (..),
  )
where

import Data.Morpheus.Server.Types.Kind (DerivingKind)
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    packName,
  )
import Data.Text (init, last, pack)
import GHC.Generics
  ( M1 (..),
    Meta,
    S,
    Selector,
    selName,
  )
import Relude hiding (init, last, undefined)
import Prelude (undefined)

selNameProxy :: forall f (s :: Meta). (Selector s) => f s -> FieldName
selNameProxy _ = fromHaskellName $ selName (undefined :: M1 S s f a)

fromHaskellName :: String -> FieldName
fromHaskellName hsName
  | not (null hsName) && (last name == '\'') = packName (init name)
  | otherwise = packName name
  where
    name = pack hsName
{-# INLINE fromHaskellName #-}

newtype ContextValue (kind :: DerivingKind) a = ContextValue
  { unContextValue :: a
  }
