module Data.Morpheus.Generics.Utils
  ( typeOf
  ) where

import           Data.Data  (Typeable, tyConName, typeRep, typeRepTyCon)
import           Data.Proxy (Proxy)
import           Data.Text  (Text, pack)

typeOf :: Typeable a => Proxy a -> Text
typeOf = pack . tyConName . typeRepTyCon . typeRep
