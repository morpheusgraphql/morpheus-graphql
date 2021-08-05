{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Base
  ( Ref (..),
    Position (..),
    Description,
    Token,
    TRUE,
    FALSE,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON (..),
  )
import Language.Haskell.TH.Syntax
  ( Lift (..),
  )
import Relude hiding
  ( ByteString,
    decodeUtf8,
    intercalate,
  )

type TRUE = 'True

type FALSE = 'False

-- Strings
type Token = Text

-- Description
type Description = Text

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving
    ( Show,
      Ord,
      Generic,
      FromJSON,
      ToJSON,
      Lift
    )

-- Positions 2 Value with same structure
-- but different Positions should be Equal
instance Eq Position where
  _ == _ = True

-- | Document Reference with its Position
--
-- Position is used only for error messages. that means:
--
-- Ref "a" 1 === Ref "a" 3
data Ref name = Ref
  { refName :: name,
    refPosition :: Position
  }
  deriving (Show, Lift, Eq)

instance Ord name => Ord (Ref name) where
  compare (Ref x _) (Ref y _) = compare x y
