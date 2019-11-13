{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Data.Morpheus.Types.Internal.Base
  ( Key
  , Collection
  , Ref(..)
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

-- includes position for debugging, where Ref "a" 1 === Ref "a" 3
data Ref = Ref
  { refName     :: Key
  , refPosition :: Position
  } deriving (Show,Lift)

instance Eq Ref where
  (Ref id1 _) == (Ref id2 _) = id1 == id2

instance Ord Ref where
  compare (Ref x _) (Ref y _) = compare x y


enhanceKeyWithNull :: Key -> Ref
enhanceKeyWithNull refName = Ref { refName, refPosition = Location 0 0 }
