{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Generics.GDecodeEnum
  ( GDecodeEnum(..)
  ) where

import           Data.Proxy   (Proxy (..))
import qualified Data.Text    as T
import           GHC.Generics

class GDecodeEnum f where
  gToEnum :: T.Text -> f a
  tagName :: Proxy f -> T.Text
  getTags :: Proxy f -> [T.Text]

instance (Datatype c, GDecodeEnum f) => GDecodeEnum (M1 D c f) where
  gToEnum = M1 . gToEnum
  tagName _ = ""
  getTags _ = getTags (Proxy :: Proxy f)

instance (Constructor c) => GDecodeEnum (M1 C c U1) where
  gToEnum _ = M1 U1
  tagName _ = T.pack $ conName (undefined :: (M1 C c U1 x))
  getTags proxy = [tagName proxy]

instance (GDecodeEnum a, GDecodeEnum b) => GDecodeEnum (a :+: b) where
  gToEnum name =
    if tagName (Proxy :: Proxy a) == name
      then L1 $ gToEnum name
      else R1 $ gToEnum name
  tagName _ = ""
  getTags _ = getTags (Proxy :: Proxy a) ++ getTags (Proxy :: Proxy b)
