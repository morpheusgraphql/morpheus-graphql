{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Data.Morpheus.Types.Internal.Base
  ( Key
  , Collection
  , Reference(..)
  , Position
  , Location(..)
  , Message
  , enhanceKeyWithNull
  )
where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Language.Haskell.TH.Syntax     ( Lift )
import           Instances.TH.Lift              ( )


type Position = Location

data Location = Location
  { line   :: Int
  , column :: Int
  } deriving (Show, Generic, FromJSON, ToJSON, Lift)

type Key = Text
type Message = Text

type Collection a = [(Key, a)]

-- includes position for debugging, where Reference "a" 1 === Reference "a" 3
data Reference = Reference
  { refName     :: Key
  , refPosition :: Position
  } deriving (Show,Lift)

instance Eq Reference where
  (Reference id1 _) == (Reference id2 _) = id1 == id2

instance Ord Reference where
  compare (Reference x _) (Reference y _) = compare x y


enhanceKeyWithNull :: Key -> Reference
enhanceKeyWithNull refName = Reference { refName, refPosition = Location 0 0 }
