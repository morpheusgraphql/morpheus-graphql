{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Types.Internal.Base
  ( Key
  , Collection
  , Reference (..)
  , Position
  , EnhancedKey(..)
  , Location(..)
  , Message
  , enhanceKeyWithNull
  ) where

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           Language.Haskell.TH.Syntax      (Lift (..))

-- MORPHEUS
import           Data.Morpheus.Types.Internal.TH (apply, liftText)

type Position = Location

data Location = Location
  { line   :: Int
  , column :: Int
  } deriving (Show, Generic, FromJSON, ToJSON, Lift)

type Key = Text
type Message = Text

type Collection a = [(Key, a)]

data Reference = Reference
  { referenceName     :: Key
  , referencePosition :: Position
  } deriving (Show)

instance Lift Reference where
  lift (Reference name pos) = apply 'Reference [liftText name, lift pos]

-- Text value that includes position for debugging, where EnhancedKey "a" 1 === EnhancedKey "a" 3
data EnhancedKey = EnhancedKey
  { uid      :: Text
  , location :: Position
  } deriving (Show)

instance Eq EnhancedKey where
  (EnhancedKey id1 _) == (EnhancedKey id2 _) = id1 == id2

instance Ord EnhancedKey where
  compare (EnhancedKey x _) (EnhancedKey y _) = compare x y

enhanceKeyWithNull :: Key -> EnhancedKey
enhanceKeyWithNull text = EnhancedKey {uid = text, location = Location 0 0}
