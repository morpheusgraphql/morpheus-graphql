{-# LANGUAGE DataKinds #-}

module Data.Morpheus.Types.Internal.AST.Stage
  ( CONST,
    VALID,
    RAW,
    Stage (..),
  )
where

type CONST = 'CONST

type VALID = 'VALID

type RAW = 'RAW

data Stage
  = RAW
  | CONST
  | VALID
