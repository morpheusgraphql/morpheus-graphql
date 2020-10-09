{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

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

type family CONST_OR_VALID (a :: Stage) :: Stage

type instance CONST_OR_VALID RAW = CONST

type instance CONST_OR_VALID CONST = CONST

type instance CONST_OR_VALID VALID = VALID
