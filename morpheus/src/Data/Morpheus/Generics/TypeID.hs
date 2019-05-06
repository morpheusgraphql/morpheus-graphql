{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Generics.TypeID where

import           Data.Proxy   (Proxy)
import           Data.Text    (Text, pack)
import           GHC.Generics

typeId ::
     forall a. (TypeID (Rep a), Generic a)
  => Proxy a
  -> Text
typeId _ = key $ from (undefined :: a)

class TypeID f where
  key :: f a -> Text

instance (Datatype c, TypeID f) => TypeID (M1 D c f) where
  key m@(M1 _) = pack $ datatypeName m

instance (Constructor c) => TypeID (M1 C c U1) where
  key _ = ""
