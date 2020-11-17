{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Stage
  ( CONST,
    VALID,
    RAW,
    Stage (..),
    CONST_OR_VALID,
  )
where

type CONST = 'CONST

type VALID = 'VALID

type RAW = 'RAW

data Stage
  = RAW
  | CONST
  | VALID

type family CONST_OR_VALID (a :: Stage) :: Stage where
  CONST_OR_VALID VALID = VALID
  CONST_OR_VALID a = CONST
