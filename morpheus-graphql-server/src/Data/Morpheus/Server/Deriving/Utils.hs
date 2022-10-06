{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils
  ( conNameProxy,
    isRecordProxy,
    selNameProxy,
    ConsRep (..),
    FieldRep (..),
    isEmptyConstraint,
    isUnionRef,
    fieldTypeName,
    unpackMonad,
    symbolName,
    DataType (..),
    DeriveWith (..),
    DeriveTypeOptions (..),
    deriveTypeWith,
  )
where

import Data.Morpheus.Server.Deriving.Utils.DeriveGType
  ( DeriveTypeOptions (..),
    DeriveWith (..),
    deriveTypeWith,
  )
import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( conNameProxy,
    isRecordProxy,
    selNameProxy,
    symbolName,
  )
import Data.Morpheus.Server.Deriving.Utils.Types
  ( ConsRep (..),
    DataType (..),
    FieldRep (..),
    fieldTypeName,
    isEmptyConstraint,
    isUnionRef,
    unpackMonad,
  )
