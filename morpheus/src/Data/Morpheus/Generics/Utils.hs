module Data.Morpheus.Generics.Utils
  ( typeOf
  , SelOf
  , RecSel
  ) where

import           Data.Data    (Typeable, tyConName, typeRep, typeRepTyCon)
import           Data.Proxy   (Proxy)
import           Data.Text    (Text, pack)
import           GHC.Generics

typeOf :: Typeable a => Proxy a -> Text
typeOf = pack . tyConName . typeRepTyCon . typeRep

type SelOf s = M1 S s (Rec0 ()) ()

type RecSel s a = M1 S s (Rec0 a)
