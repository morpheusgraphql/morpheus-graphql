{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Resolve.Generics.TypeID
  ( __typeName
  , __typeId
  , TypeID
  ) where

import           Data.Proxy   (Proxy)
import           Data.Text    (Text, pack)
import           GHC.Generics

__typeName ::
     forall a. (TypeID (Rep a), Generic a)
  => Proxy a
  -> Text
__typeName _ = key $ from (undefined :: a)

__typeId ::
     forall a. (TypeID (Rep a), Generic a)
  => Proxy a
  -> Text
__typeId _ = typeUniqueId $ from (undefined :: a)

keyName :: Datatype c => M1 D c f a -> Text
keyName m@(M1 _) = pack (datatypeName m)

typeLocation :: Datatype c => M1 D c f a -> Text
typeLocation m@(M1 _) = pack (moduleName m ++ "." ++ packageName m)

class TypeID f where
  key :: f a -> Text
  typeUniqueId :: f a -> Text

instance Datatype c => TypeID (M1 D c f) where
  key = keyName
  typeUniqueId val = typeLocation val <> "." <> keyName val
