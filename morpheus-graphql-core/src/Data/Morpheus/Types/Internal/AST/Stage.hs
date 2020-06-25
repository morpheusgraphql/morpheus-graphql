{-# LANGUAGE DataKinds #-}

module Data.Morpheus.Types.Internal.AST.Stage
  ( RESOLVED,
    VALID,
    RAW,
    Stage (..),
  )
where

type RESOLVED = 'RESOLVED

type VALID = 'VALID

type RAW = 'RAW

data Stage = RAW | RESOLVED | VALID
