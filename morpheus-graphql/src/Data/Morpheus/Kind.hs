{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | associating types to GraphQL Kinds
module Data.Morpheus.Kind
  ( SCALAR,
    WRAPPER,
    DerivingKind (..),
    TYPE,
    CUSTOM,
  )
where

import Data.Morpheus.Server.Types
  ( CUSTOM,
    DerivingKind (..),
    SCALAR,
    TYPE,
    WRAPPER,
  )
