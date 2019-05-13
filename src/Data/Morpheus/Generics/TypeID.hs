{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

--{-# LANGUAGE OverloadedStrings   #-}
module Data.Morpheus.Generics.TypeID
  ( typeId
  , TypeID
  ) where

import           Data.Proxy   (Proxy)
import           Data.Text    (Text, pack)
import           GHC.Generics

typeId ::
     forall a. (TypeID (Rep a), Generic a)
  => Proxy a
  -> Text
typeId _ = key $ from (undefined :: a)

keyName :: Datatype c => M1 D c f a -> Text
keyName m@(M1 _) = pack $ datatypeName m

class TypeID f where
  key :: f a -> Text

instance Datatype c => TypeID (M1 D c f) where
  key = keyName
{--

instance (Constructor c) => TypeID (M1 C c f) where
  key _ = ""
instance (Selector c) => TypeID (M1 S c f) where
  key _ = ""
instance TypeID (f :*: g) where
  key _ = ""
instance TypeID (f :+: g) where
  key _ = ""

--}
