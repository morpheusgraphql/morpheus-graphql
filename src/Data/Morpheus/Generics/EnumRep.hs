{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Generics.EnumRep
  ( EnumRep(..)
  ) where

import           Data.Proxy   (Proxy (..))
import           Data.Text    (Text, pack)
import           GHC.Generics

class EnumRep f where
  encodeRep :: f a -> Text
    --typeName :: Text -> f a -> Text
  gToEnum :: Text -> f a
  tagName :: Proxy f -> Text
  getTags :: Proxy f -> [Text]

instance (Datatype c, EnumRep f) => EnumRep (M1 D c f) where
  encodeRep (M1 src) = encodeRep src
    --typeName _ m@(M1 src) = typeName (pack $ datatypeName m) src
  gToEnum = M1 . gToEnum
  tagName _ = ""
  getTags _ = getTags (Proxy :: Proxy f)

instance (Constructor c) => EnumRep (M1 C c U1) where
  encodeRep m@(M1 _) = pack $ conName m
    --typeName key' _ = key'
  gToEnum _ = M1 U1
  tagName _ = pack $ conName (undefined :: (M1 C c U1 x))
  getTags proxy = [tagName proxy]

instance (EnumRep a, EnumRep b) => EnumRep (a :+: b) where
  encodeRep (L1 x) = encodeRep x
  encodeRep (R1 x) = encodeRep x
  --typeName key' _ = key'
  gToEnum name =
    if tagName (Proxy @a) == name
      then L1 $ gToEnum name
      else R1 $ gToEnum name
  tagName _ = ""
  getTags _ = getTags (Proxy @a) ++ getTags (Proxy @b)
