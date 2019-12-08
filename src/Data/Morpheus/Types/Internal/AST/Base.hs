{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE DataKinds       #-}

module Data.Morpheus.Types.Internal.AST.Base
  ( Key
  , Collection
  , Ref(..)
  , Position(..)
  , Message
  , anonymousRef
  , Name
  , Description
  , VALID
  , RAW
  )
where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Language.Haskell.TH.Syntax     ( Lift )
import           Instances.TH.Lift              ( )


type Key = Text
type Message = Text
type Name = Key
type Description = Key

type Collection a = [(Key, a)]

type VALID = 'True
type RAW = 'False

data Position = Position
  { line   :: Int
  , column :: Int
  } deriving (Show, Generic, FromJSON, ToJSON, Lift)

-- Refference with Position information  
--
-- includes position for debugging, where Ref "a" 1 === Ref "a" 3
--
data Ref = Ref
  { refName     :: Key
  , refPosition :: Position
  } deriving (Show,Lift)

instance Eq Ref where
  (Ref id1 _) == (Ref id2 _) = id1 == id2

instance Ord Ref where
  compare (Ref x _) (Ref y _) = compare x y

anonymousRef :: Key -> Ref
anonymousRef refName = Ref { refName, refPosition = Position 0 0 }
