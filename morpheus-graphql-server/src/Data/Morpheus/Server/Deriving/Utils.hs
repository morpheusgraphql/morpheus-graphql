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
    toFieldRes,
    DataType (..),
    DeriveWith (..),
    DeriveTypeOptions (..),
    deriveTypeWith,
  )
where

import Data.Morpheus.Server.Deriving.Utils.DeriveGType
import Data.Morpheus.Server.Deriving.Utils.Proxy
import Data.Morpheus.Server.Deriving.Utils.Types
